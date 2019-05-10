class ZCL_FILE_SERVICES definition
  public
  create public .

public section.

  types:
    mtty_files TYPE STANDARD TABLE OF file_info WITH EMPTY KEY .

  constants MC_PATH_SEPARATOR type CHAR1 value '\' ##NO_TEXT.
  constants MC_SEPARATOR type CHAR2 value '%%' ##NO_TEXT.
  constants MC_SIMSCORE_FILE type STRING value 'K:\Temp\ImageGallery\Scoring\SimilarityScore.json' ##NO_TEXT.
  constants MC_PREFIX type CHAR1 value '_' ##NO_TEXT.
  constants MC_EXT_SEPARATOR type CHAR1 value '.' ##NO_TEXT.

  class-methods DECODE_FILENAME
    importing
      !IV_FILENAME type CLIKE
    returning
      value(RV_FILENAME) type STRING .
  class-methods ENCODE_FILENAME
    importing
      !IV_FILENAME type CLIKE
    returning
      value(RV_FILENAME) type STRING .
  class-methods SPLIT_FILENAME
    importing
      !IV_FILENAME type CLIKE
    exporting
      !EV_FILENAME type CLIKE
      !EV_FILEEXT type CLIKE .
  class-methods XSTRING_TO_STRING
    importing
      !IV_XSTRING type XSTRING
    returning
      value(ER_STRING) type STRING
    raising
      CX_CONVERSION_FAILED .
  class-methods FILE_OPEN_DIALOG
    importing
      !IV_PATH_W_FILE type CLIKE
    returning
      value(ER_PATH_W_FILE) type RLGRAP-FILENAME .
  class-methods READ_FILE
    importing
      !IV_PATH_W_FILE type CLIKE
    returning
      value(RV_FILE_CONTENT) type STRING
    raising
      CX_RSL_UI_UPLOAD_ERROR .
  class-methods SPLIT_PATH_TO_FILENAME
    importing
      !IV_FILEPATH type CLIKE
    exporting
      !EV_PATHNAME type CLIKE
      !EV_FILENAME type CLIKE .
  class-methods GET_FILES
    importing
      !IV_GALLERY_DIRECTORY type CLIKE
    exporting
      !ET_FILES type MTTY_FILES
    exceptions
      NO_FILES_FOUNDS .
  class-methods GET_DIR
    importing
      !IV_INITIAL_FOLDER type CLIKE
    returning
      value(RV_SELECTED_FOLDER) type STRING .
  class-methods CREATE_ZIP
    raising
      CX_SY_CREATE_OBJECT_ERROR .
  class-methods ADD_TO_ZIP
    importing
      !IO_IMAGE type ref to ZCL_IMAGE
    raising
      CX_CONVERSION_FAILED .
  class-methods CLOSE_ZIP
    returning
      value(RV_ZIP_FILE_XCONTENT) type XSTRING .
  class-methods COUNT_FILES_IN_ZIP
    returning
      value(RV_FILES_IN_ZIP) type I .
  class-methods GET_ZIP
    returning
      value(RV_ZIP_FILE_XCONTENT) type XSTRING .
  class-methods SAVE_FILE
    importing
      !IV_FILENAME type CLIKE optional
      !IV_CONTENT type CLIKE
      !IV_ASK_OVERWRITE type BOOLEAN default SPACE
    raising
      CX_TPDA_FILE_DOWNLOAD .
protected section.
private section.

  class-data MO_ZIP type ref to CL_ABAP_ZIP .
  class-data MV_ZIP_SERVICE_AVAIABLE type BOOLEAN .
  class-data MV_FILES_COUNT type I .
  class-data MV_ZIP_FILE_XCONTENT type XSTRING .
ENDCLASS.



CLASS ZCL_FILE_SERVICES IMPLEMENTATION.


  METHOD add_to_zip.
* https://sap4tech.net/conversion-abap-binary-string-xstring/
* https://wiki.scn.sap.com/wiki/display/ABAP/Zip+any+file+via+ABAP+using+CL_ABAP_ZIP
*
*
    DATA: lv_vector   TYPE string,
          lv_filename TYPE string,
          lv_xcontent TYPE xstring.
*
    lv_vector = io_image->features( iv_parsed = abap_true ).
    CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
      EXPORTING
        text   = lv_vector
*       MIMETYPE       = ' '
*       ENCODING       = ENCODING
      IMPORTING
        buffer = lv_xcontent
      EXCEPTIONS
        failed = 1
        OTHERS = 2.
    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE cx_conversion_failed.
    ENDIF.
*
    lv_filename = io_image->filename( ).
    lv_filename = encode_filename( lv_filename ).
    mo_zip->add( name    = lv_filename
                 content = lv_xcontent ).
*
    ADD 1 TO mv_files_count.
*
  ENDMETHOD.


  METHOD close_zip.
*
*   BREAK-POINT ID zbpid_filesrv.
    mv_zip_file_xcontent = mo_zip->save( ).
    rv_zip_file_xcontent = mv_zip_file_xcontent.
*
  ENDMETHOD.


  METHOD count_files_in_zip.
    rv_files_in_zip = mv_files_count.
  ENDMETHOD.


  METHOD create_zip.
*
*   BREAK-POINT ID zbpid_filesrv.
    IF mo_zip IS BOUND AND mv_zip_service_avaiable = abap_true.
      CLEAR: mv_files_count, mv_zip_file_xcontent.
      RETURN.
    ENDIF.
*
    IF mo_zip IS NOT BOUND.
      CREATE OBJECT mo_zip.
      IF mo_zip IS BOUND. mv_zip_service_avaiable = abap_true. ENDIF.
    ENDIF.
*
    IF mv_zip_service_avaiable EQ abap_false.
      RAISE EXCEPTION TYPE cx_sy_create_object_error.
    ENDIF.
*
  ENDMETHOD.


  METHOD decode_filename.
*
*   BREAK-POINT ID zbpid_filesrv.
    rv_filename = iv_filename.
    SHIFT rv_filename LEFT DELETING LEADING mc_prefix.
    REPLACE mc_separator WITH mc_ext_separator INTO rv_filename.
*
  ENDMETHOD.


  METHOD encode_filename.
*
    DATA: lv_filename TYPE string,
          lv_fileext  TYPE string.
*
*   BREAK-POINT id zbpid_filesrv.
    zcl_file_services=>split_filename(  EXPORTING iv_filename = iv_filename
                                        IMPORTING ev_filename = lv_filename
                                                  ev_fileext  = lv_fileext ).

    rv_filename = |{ mc_prefix }{ lv_filename }{ mc_separator }{ lv_fileext }|.
*
  ENDMETHOD.


  METHOD file_open_dialog.
*
    DATA: lv_directory     TYPE string,
          lv_filename_wext TYPE string.
*
    DATA: lt_file_table    TYPE filetable,
          ls_selected_file LIKE LINE OF lt_file_table,
*
          lv_rc            TYPE i,
          lv_user_action   TYPE i,
          lv_file_encoding TYPE abap_encoding.
*
*   BREAK-POINT ID zbpid_filesrv.
    er_path_w_file = iv_path_w_file.
    IF iv_path_w_file IS INITIAL. RETURN. ENDIF.
*
    er_path_w_file = iv_path_w_file.
*
    zcl_file_services=>split_path_to_filename( EXPORTING iv_filepath = iv_path_w_file
                                               IMPORTING ev_pathname = lv_directory
                                                         ev_filename = lv_filename_wext ).
*
    cl_gui_frontend_services=>file_open_dialog( EXPORTING default_filename        = lv_filename_wext
                                                          initial_directory       = lv_directory
                                                          multiselection          = abap_false
"                                                         window_title            = window_title
"                                                         default_extension       = default_extension
                                                          file_filter             = |*.txt,*.json,*.jpg|
"                                                         with_encoding           = with_encoding
                                                 CHANGING file_table              = lt_file_table
                                                          rc                      = lv_rc
                                                          user_action             = lv_user_action
                                                          file_encoding           = lv_file_encoding
                                                 EXCEPTIONS OTHERS                = 99 ).
    IF sy-subrc IS NOT INITIAL OR lt_file_table[] IS INITIAL OR
       lv_user_action NE cl_gui_frontend_services=>action_ok. RETURN.
    ENDIF.
*
    READ TABLE lt_file_table INTO ls_selected_file INDEX 1.
    IF sy-subrc IS NOT INITIAL OR ls_selected_file-filename IS INITIAL.
      RETURN.
    ENDIF.
*
    er_path_w_file = ls_selected_file-filename.
*
  ENDMETHOD.


  METHOD get_dir.
*
    rv_selected_folder = iv_initial_folder.
    cl_gui_frontend_services=>directory_browse( EXPORTING window_title    = space
                                                          initial_folder  = rv_selected_folder
                                                CHANGING  selected_folder = rv_selected_folder ).
    rv_selected_folder = rv_selected_folder && mc_path_separator.
*
  ENDMETHOD.


  METHOD get_files.
*
    DATA: lv_directory TYPE string,
          lt_files     TYPE zcl_file_services=>mtty_files,
          lv_count     TYPE i.
*
*   Get Files from directory
    lv_directory = iv_gallery_directory.
    cl_gui_frontend_services=>directory_list_files( EXPORTING directory   = lv_directory
                                                              files_only  = abap_true
                                                    CHANGING  file_table  = lt_files
                                                              count       = lv_count
                                                    EXCEPTIONS cntl_error                  = 1
                                                               directory_list_files_failed = 2
                                                               wrong_parameter             = 3
                                                               error_no_gui                = 4
                                                               not_supported_by_gui        = 5
                                                               OTHERS                      = 6 ).
    IF sy-subrc IS NOT INITIAL.
      RAISE no_files_founds.
    ENDIF.
*
    LOOP AT lt_files ASSIGNING FIELD-SYMBOL(<fs_file>).
      IF <fs_file>-filelength IS INITIAL.   "Filter Empty Files
        DELETE lt_files WHERE filename = <fs_file>-filename. CONTINUE.
      ENDIF.
      <fs_file>-filename = iv_gallery_directory && <fs_file>-filename.
    ENDLOOP.
    IF lt_files[] IS INITIAL.
      RAISE no_files_founds.
    ENDIF.
*
    et_files[] = lt_files[].
*
  ENDMETHOD.


  METHOD get_zip.
    rv_zip_file_xcontent = mv_zip_file_xcontent.
  ENDMETHOD.


  METHOD read_file.
*
    DATA: lv_filename   TYPE string,
          lv_filelength TYPE i,
          lt_data_tab   TYPE STANDARD TABLE OF string,
          lv_data       LIKE LINE OF lt_data_tab.
*
*   BREAK-POINT ID zbpid_filesrv.
    lv_filename = iv_path_w_file.
    cl_gui_frontend_services=>gui_upload( EXPORTING  filename   = lv_filename
                                                     filetype   = 'ASC'
                                          IMPORTING  filelength = lv_filelength
                                          CHANGING   data_tab   = lt_data_tab
                                          EXCEPTIONS OTHERS     = 99 ).
    IF sy-subrc IS NOT INITIAL OR lt_data_tab[] IS INITIAL.
      RAISE EXCEPTION TYPE cx_rsl_ui_upload_error.
    ENDIF.
*
    READ TABLE lt_data_tab INTO lv_data INDEX 1.
    IF sy-subrc IS NOT INITIAL OR lv_data IS INITIAL.
      RAISE EXCEPTION TYPE cx_rsl_ui_upload_error.
    ENDIF.
*
    rv_file_content = lv_data.
*
  ENDMETHOD.


  METHOD save_file.
*
    DATA: lv_filename TYPE string,
          lt_data_tab TYPE STANDARD TABLE OF string.
*
*   BREAK-POINT ID zbpid_filesrv.
    lv_filename = mc_simscore_file.
    IF iv_filename IS SUPPLIED AND iv_filename NE lv_filename. lv_filename = iv_filename. ENDIF.
*
    APPEND iv_content TO lt_data_tab.
    cl_gui_frontend_services=>gui_download( EXPORTING filename          = lv_filename
                                                      filetype          = 'ASC'
                                                      write_lf          = abap_true
                                                      confirm_overwrite = iv_ask_overwrite
                                                      no_auth_check     = abap_true
"                                                     codepage          = SPACE
                                            CHANGING  data_tab          = lt_data_tab
                                            EXCEPTIONS OTHERS           = 24 ).
    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE cx_tpda_file_download.
    ENDIF.
*
  ENDMETHOD.


  METHOD split_filename.
*
    DATA: lv_long_filename  TYPE dbmsgora-filename,
          lv_pure_filename  TYPE  sdbah-actid,
          lv_pure_extension TYPE  sdbad-funct.
*
    lv_long_filename = iv_filename.
    CALL FUNCTION 'SPLIT_FILENAME'
      EXPORTING
        long_filename  = lv_long_filename
      IMPORTING
        pure_filename  = lv_pure_filename
        pure_extension = lv_pure_extension.
*
    ev_filename = lv_pure_filename.
    ev_fileext  = lv_pure_extension.
*
     endmethod.


  METHOD split_path_to_filename.
*
    DATA: lv_filepath TYPE rsfilenm,
          lv_pathname TYPE rstxtlg,
          lv_filename TYPE rsawbnobjnm.
*
    lv_filepath = iv_filepath.
    CALL FUNCTION 'RSDS_SPLIT_PATH_TO_FILENAME'
      EXPORTING
        i_filepath = lv_filepath
      IMPORTING
        e_pathname = lv_pathname
        e_filename = lv_filename.
*
    ev_pathname = lv_pathname.
    ev_filename = lv_filename.
*
  ENDMETHOD.


  METHOD xstring_to_string.
*
    DATA: lo_conv    TYPE REF TO cl_abap_conv_in_ce,
          lv_xstring TYPE xstring,
          lv_string  TYPE string.
*
    lv_xstring = iv_xstring.

    TRY.
        lo_conv = cl_abap_conv_in_ce=>create( EXPORTING  input       = lv_xstring
    "                                                    encoding    = 'UTF-8'
    "                                                    replacement = '?'
                                                         ignore_cerr = abap_true ).
        IF lo_conv IS NOT BOUND. RAISE EXCEPTION TYPE cx_conversion_failed. ENDIF.
*
        lo_conv->read( IMPORTING data = lv_string ).
        IF lv_string IS INITIAL. RAISE EXCEPTION TYPE cx_conversion_failed. ENDIF.
*
        er_string = lv_string.
*
      CATCH cx_root.
        RAISE EXCEPTION TYPE cx_conversion_failed.
    ENDTRY.
*
  ENDMETHOD.
ENDCLASS.
