class ZCL_IMAGE definition
  public
  create public .

public section.
  type-pools ABAP .

  class-methods BUILD_PICTURE_URL
    importing
      !IV_XCONTENT type XSTRING
      !IV_MIMETYPE type CLIKE
    returning
      value(RV_PICTURE_URL) type CHAR255 .
  methods CONSTRUCTOR
    importing
      !IV_FILENAME type CLIKE optional
      !IS_IMAGE_DATA type ZTB_GALLERY optional
    raising
      CX_SMIME .
  methods FILEEXT
    returning
      value(RV_FILEEXT) type STRING .
  methods FILENAME
    importing
      !IV_NO_EXTENSION type BOOLEAN default SPACE
    returning
      value(RV_FILENAME) type STRING .
  methods MIMETYPE
    returning
      value(RV_MIMETYPE) type SKWF_MIME .
  methods SIZE
    returning
      value(RV_FILESIZE) type I .
  methods VALPOS
    importing
      !IV_FOR_UPDATE type BOOLEAN default SPACE
    returning
      value(RV_VALPOS) type VALPOS .
  methods CONTENT
    returning
      value(RV_FILECONTENT) type XSTRING .
  methods SET_FEATURES
    importing
      !IV_FEATURES type STRING .
  methods FEATURES
    importing
      !IV_PARSED type BOOLEAN default ABAP_FALSE
    returning
      value(RV_FEATURES) type STRING .
  methods IS_VALID
    returning
      value(RV_IS_VALID) type BOOLEAN .
  methods MD5
    returning
      value(RV_HASH) type HASH160 .
protected section.
private section.

  constants MC_MIME_IMAGE type SKWF_MIME value 'image' ##NO_TEXT.
  data MV_DIRECTORY type STRING .
  data MV_FILENAME_WEXT type STRING .
  data MV_FILEEXT type STRING .
  data MV_FILENAME type STRING .
  data MV_FILESIZE type I .
  data MV_LONG_FILENAME type STRING .
  data MV_MIMETYPE type SKWF_MIME .
  data MV_VALPOS type VALPOS .
  data MV_FILECONTENT type XSTRING .
  data MV_FEATURES type STRING .
  data MV_VALID_FEATURES type BOOLEAN .
  data MV_MD5_HASH type HASH160 .
  data MV_INIT_WITH_DATA type BOOLEAN .
  data MO_IMAGE_PROCESSOR type ref to CL_FXS_IMAGE_PROCESSOR .
  data MV_HANDLE type I .

  methods _VALPOS .
  methods _UPLOAD_CONTENT
    raising
      CX_RSL_UI_UPLOAD_ERROR
      CX_CONVERSION_FAILED .
  methods _FILE_INFO
    importing
      !IV_FILENAME type CLIKE
    raising
      CX_SMIME .
  methods _MD5
    raising
      CX_CTS_BUFFER_CALC .
  methods _CREATE_WITH
    importing
      !IS_IMAGE_DATA type ZTB_GALLERY .
ENDCLASS.



CLASS ZCL_IMAGE IMPLEMENTATION.


  METHOD build_picture_url.
*
    TYPES: lty_pict_line   TYPE x LENGTH 1022,
           ltty_pict_lines TYPE STANDARD TABLE OF lty_pict_line.
*
    DATA: lv_length   TYPE i,
          lv_xcontent TYPE xstring.
*
    DATA lt_picture_table TYPE ltty_pict_lines.
    DATA: lv_url     LIKE rv_picture_url,
          lv_subtype TYPE c LENGTH 3.
*
    BREAK-POINT ID zbpid_image.
    lv_xcontent = iv_xcontent.
    lv_length = xstrlen( lv_xcontent ).
    WHILE lv_length >= 1022.
      APPEND lv_xcontent(1022) TO lt_picture_table.
      SHIFT lv_xcontent BY 1022 PLACES LEFT IN BYTE MODE.
      lv_length = xstrlen( lv_xcontent ).
    ENDWHILE.
    IF lv_length > 0.
      APPEND lv_xcontent TO lt_picture_table.
    ENDIF.
*
    lv_subtype = to_upper( iv_mimetype ).
    CALL FUNCTION 'DP_CREATE_URL'
      EXPORTING
        type                = 'IMAGE'
        subtype             = lv_subtype
        cacheable           = abap_true
        send_data_as_string = abap_true
      TABLES
        data                = lt_picture_table
      CHANGING
        url                 = lv_url.
*
    rv_picture_url = lv_url.
*
  ENDMETHOD.


  METHOD constructor.
*
    DATA lo_exception TYPE REF TO cx_root.
*
    DATA: lv_converted_data TYPE xstring,
          lv_mimetype       TYPE string,
          lv_xres_width     TYPE i,
          lv_yres_height    TYPE i,
          lv_xdpi           TYPE i,
          lv_ydpi           TYPE i,
          lv_bitdepth       TYPE i.
*
*   BREAK-POINT ID zbpid_image.
    IF is_image_data IS SUPPLIED. mv_init_with_data = abap_true. ENDIF.
*
    CASE mv_init_with_data.
*
      WHEN abap_false.
        TRY.
            _file_info( iv_filename ).
            _upload_content( ).
            _md5( ).
*
          CATCH cx_rsl_ui_upload_error
                cx_conversion_failed INTO lo_exception.
            RETURN.
*
          CATCH cx_cts_buffer_calc INTO lo_exception.
            RETURN.
*
        ENDTRY.
*
      WHEN abap_true.
        _create_with( is_image_data ).
    ENDCASE.
    RETURN.
*
*   http://serkanozcan.com/blog/sap-abap-resize-image/
    TRY.
        CREATE OBJECT mo_image_processor.
        mv_handle = mo_image_processor->add_image( iv_data        = mv_filecontent
                                                   iv_image_name =  mv_filename_wext ).
*
        mo_image_processor->get_info( EXPORTING iv_handle   = mv_handle
                                      IMPORTING ev_mimetype = lv_mimetype
                                                ev_xres     = lv_xres_width
                                                ev_yres     = lv_yres_height
                                                ev_xdpi     = lv_xdpi
                                                ev_ydpi     = lv_ydpi
                                                ev_bitdepth = lv_bitdepth ).
*
*       .....
        lv_converted_data = mo_image_processor->get_image( EXPORTING iv_handle = mv_handle ).
*
      CATCH cx_fxs_image_unsupported.
* ...
      CATCH cx_sy_range_out_of_bounds .
* ...
    ENDTRY.
*
  ENDMETHOD.


  METHOD content.
    rv_filecontent = mv_filecontent.
  ENDMETHOD.


  METHOD features.
*
*   BREAK-POINT ID zbpid_image.
    rv_features = mv_features.
    IF iv_parsed EQ abap_true.
      rv_features = zcl_scp_api=>get_features( mv_features ).
    ENDIF.
*
  ENDMETHOD.


  METHOD fileext.
    rv_fileext = mv_fileext.
  ENDMETHOD.


  METHOD filename.
*
    rv_filename = mv_filename_wext.
    IF iv_no_extension EQ abap_true.
      rv_filename = mv_filename.
    ENDIF.
*
  ENDMETHOD.


  METHOD is_valid.
    rv_is_valid = mv_valid_features.
  ENDMETHOD.


  METHOD md5.
    rv_hash = mv_md5_hash.
  ENDMETHOD.


  METHOD MIMETYPE.
    rv_mimetype = mv_mimetype.
  ENDMETHOD.


  METHOD set_features.
*
*   BREAK-POINT ID zbpid_image.
    IF iv_features IS NOT INITIAL.
      mv_features = iv_features.
      mv_valid_features = abap_true.
    ENDIF.
*
  ENDMETHOD.


  METHOD size.
    rv_filesize = mv_filesize.
  ENDMETHOD.


  METHOD valpos.
*
    rv_valpos = mv_valpos.
    IF iv_for_update EQ abap_false. RETURN. ENDIF.
*
    _valpos( ).
    rv_valpos = mv_valpos.
*
  ENDMETHOD.


  METHOD _create_with.
*
*   BREAK-POINT ID zbpid_image.
    mv_long_filename =
    mv_filename_wext = is_image_data-filename.
    mv_mimetype      = is_image_data-mimetype.
*
* >>>
*    lv_long_filename = mv_filename_wext.
*    CALL FUNCTION 'SPLIT_FILENAME'
*      EXPORTING
*        long_filename  = lv_long_filename
*      IMPORTING
*        pure_filename  = lv_pure_filename
*        pure_extension = lv_pure_extension.
*    mv_filename = lv_pure_filename.
*    mv_fileext  = lv_pure_extension.
* >>>
*
    zcl_file_services=>split_filename( EXPORTING iv_filename = mv_filename_wext
                                       IMPORTING ev_filename = mv_filename
                                                 ev_fileext  = mv_fileext ).
*
    mv_valpos   = is_image_data-valpos.
    mv_filesize = is_image_data-filesize.
    mv_md5_hash = is_image_data-md5_hash.
    mv_features = is_image_data-features.
    IF mv_features IS NOT INITIAL. mv_valid_features = abap_true. ENDIF.
    mv_filecontent = is_image_data-filecontent.
*
  ENDMETHOD.


  METHOD _file_info.
*
    DATA lv_mimetype LIKE mv_mimetype.
*
    DATA: lv_filepath TYPE rsfilenm.
*
    DATA lv_file_name TYPE skwf_filnm.
*
*   BREAK-POINT ID zbpid_image.
    lv_file_name = iv_filename.
    CALL FUNCTION 'SKWF_MIMETYPE_OF_FILE_GET'
      EXPORTING
        filename = lv_file_name
      IMPORTING
        mimetype = lv_mimetype.
    IF NOT lv_mimetype CS mc_mime_image.
      RAISE EXCEPTION TYPE cx_smime.
    ENDIF.
    mv_mimetype      = lv_mimetype.
    mv_long_filename = iv_filename.
*
* >>>
*    lv_filepath = mv_long_filename.
*    CALL FUNCTION 'RSDS_SPLIT_PATH_TO_FILENAME'
*      EXPORTING
*        i_filepath = lv_filepath
*      IMPORTING
*        e_pathname = lv_pathname
*        e_filename = lv_filename.
*    mv_directory      = lv_pathname.
*    mv_filename_wext  = lv_filename.
* >>>
*
    zcl_file_services=>split_path_to_filename( EXPORTING iv_filepath = mv_long_filename
                                               IMPORTING ev_pathname = mv_directory
                                                         ev_filename = mv_filename_wext ).
*
* >>>
*    lv_long_filename = mv_filename_wext.
*    CALL FUNCTION 'SPLIT_FILENAME'
*      EXPORTING
*        long_filename  = lv_long_filename
*      IMPORTING
*        pure_filename  = lv_pure_filename
*        pure_extension = lv_pure_extension.
*    mv_filename = lv_pure_filename.
*    mv_fileext  = lv_pure_extension.
* >>>
*
    zcl_file_services=>split_filename( EXPORTING iv_filename = mv_filename_wext
                                       IMPORTING ev_filename = mv_filename
                                                 ev_fileext  = mv_fileext ).
*
  ENDMETHOD.


  METHOD _md5.
*
    DATA lv_hash TYPE hash160.
*
*   BREAK-POINT ID zbpid_image.
    CALL FUNCTION 'CALCULATE_HASH_FOR_RAW'
      EXPORTING
        alg            = 'MD5'
        data           = mv_filecontent
        length         = mv_filesize
      IMPORTING
        hash           = lv_hash
*       HASHLEN        = HASHLEN
*       HASHX          = HASHX
*       HASHXLEN       = HASHXLEN
*       hashstring     = hashstring
*       HASHXSTRING    = HASHXSTRING
*       HASHB64STRING  = HASHB64STRING
      EXCEPTIONS
        unknown_alg    = 1
        param_error    = 2
        internal_error = 3
        OTHERS         = 4.
    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE cx_cts_buffer_calc.
    ENDIF.
*
    mv_md5_hash = lv_hash.
*

* >>>
*DATA ALG           TYPE HASHALG.
*DATA DATA          TYPE XSTRING.
*DATA LENGTH        TYPE I.
*DATA HASHLEN       TYPE I.
*DATA HASHX         TYPE HASH160X.
*DATA HASHXLEN      TYPE I.
*DATA HASHXSTRING   TYPE XSTRING.
*DATA HASHB64STRING TYPE STRING.
* >>>
  ENDMETHOD.


  METHOD _upload_content.
*
    DATA: lv_filelength TYPE i,
          lt_data_tab   TYPE STANDARD TABLE OF tbl1024.
*
*   BREAK-POINT ID zbpid_image.
    cl_gui_frontend_services=>gui_upload( EXPORTING  filename   = mv_long_filename
                                                     filetype   = 'BIN'
                                          IMPORTING  filelength = lv_filelength
                                          CHANGING   data_tab   = lt_data_tab
                                          EXCEPTIONS OTHERS     = 99 ).
    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE cx_rsl_ui_upload_error.
    ENDIF.
*
    mv_filesize = lv_filelength.
*
    CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
      EXPORTING
        input_length = lv_filelength
      IMPORTING
        buffer       = mv_filecontent
      TABLES
        binary_tab   = lt_data_tab
      EXCEPTIONS
        failed       = 1
        OTHERS       = 2.
    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE cx_conversion_failed.
    ENDIF.
*
  ENDMETHOD.


  METHOD _valpos.
*
    DATA ls_image_data TYPE ztb_gallery.
*
    SELECT * UP TO 1 ROWS INTO @ls_image_data FROM ztb_gallery
      WHERE filename = @mv_filename_wext ORDER BY valpos DESCENDING.
    ENDSELECT.
    IF sy-subrc IS INITIAL AND ls_image_data-md5_hash NE mv_md5_hash.
      ADD 1 TO mv_valpos.
    ENDIF.
*
  ENDMETHOD.
ENDCLASS.
