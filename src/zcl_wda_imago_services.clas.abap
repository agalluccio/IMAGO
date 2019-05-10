class ZCL_WDA_IMAGO_SERVICES definition
  public
  inheriting from CL_WD_COMPONENT_ASSISTANCE
  final
  create public .

public section.

  class-methods BUILD_URL
    importing
      !IV_XCONTENT type XSTRING
      !IV_MIMETYPE type STRING
      !IV_EXT type STRING
    returning
      value(RV_URL) type STRING .
  methods LOAD_FILE
    importing
      !IV_FILE_CONTENT type STRING .
  methods GET_IMAGE_GALLERY
    returning
      value(RT_IMAGES) type ZTT_IMAGES .
  methods GET_IMAGE
    importing
      !IV_WITH_FILENAME type CLIKE
    returning
      value(RS_IMAGE) type ZST_IMAGE
    raising
      CX_DM_NOT_FOUND .
  methods GET_SIMILAR_IMAGES
    importing
      !IS_IMAGE type ZST_IMAGE
    returning
      value(RT_IMAGES) type ZTT_IMAGES
    raising
      CX_DM_NOT_FOUND .
protected section.
private section.

  data MT_IMAGES type ZTT_IMAGES .
  data MT_SIMILARITY_SCORING type ZCL_SCP_API=>MTTY_SCORES .
ENDCLASS.



CLASS ZCL_WDA_IMAGO_SERVICES IMPLEMENTATION.


  METHOD build_url.
* https://archive.sap.com/discussions/thread/1579917
* https://blogs.sap.com/2013/09/27/upload-and-display-an-image-at-runtime/
* https://blogs.sap.com/2014/01/10/attach-files-with-save-retrieve-and-delete-functionality-in-web-dynpro-abap-part-1/
*
    CONSTANTS lc_cache_timeout TYPE i VALUE 60.
    CONSTANTS lc_img_path TYPE string VALUE '/sap/public/'.
*
    DATA lo_http_response TYPE REF TO if_http_response.
*
    DATA lv_uuid TYPE sysuuid_c22.
    DATA lv_url TYPE string.
*
    BREAK-POINT ID zbpid_imago.
    CREATE OBJECT lo_http_response TYPE cl_http_response
      EXPORTING
        add_c_msg = 1.
*
*   cached_response->set_compression( options = cached_response->IF_HTTP_ENTITY~CO_COMPRESS_IN_ALL_CASES ).
    TRY. " ignore, if compression can not be switched on
        lo_http_response->set_compression( EXPORTING options = lo_http_response->co_compress_based_on_mime_type
                                           EXCEPTIONS OTHERS = 1 ).
      CATCH cx_root.
    ENDTRY.

*  set the data and the headers
    lo_http_response->set_data( iv_xcontent ).
    lo_http_response->set_header_field( name  = if_http_header_fields=>content_type
                                        value = iv_mimetype ).

*   Set the Response Status
    lo_http_response->set_status( code = zcl_scp_api=>mc_http_ok reason = 'OK' ).

*   Set the Cache Timeout - 60 seconds - we only need this in the cache
*   long enough to build the page and allow the IFrame on the Client to request it.
    lo_http_response->server_cache_expire_rel( expires_rel = lc_cache_timeout ).

    TRY.
*
*       Create a unique URL for the object
        lv_uuid = cl_system_uuid=>create_uuid_c22_static( ).
        CONCATENATE lc_img_path lv_uuid INTO lv_url.

*       Cache the URL
        cl_http_server=>server_cache_upload( url      = lv_url
                                             response = lo_http_response ).
*
        rv_url = lv_url.
*
      CATCH cx_uuid_error.
        RETURN.
    ENDTRY.
*
  ENDMETHOD.


  METHOD get_image.
*
    BREAK-POINT ID zbpid_imago.
    IF iv_with_filename IS INITIAL. RETURN. ENDIF.
*
    READ TABLE mt_images INTO rs_image WITH KEY filename = iv_with_filename.
    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE cx_dm_not_found.
    ENDIF.
*
  ENDMETHOD.


  METHOD get_image_gallery.
    rt_images[] = mt_images[].
  ENDMETHOD.


  METHOD get_similar_images.
*
    DATA: lt_images             TYPE ztt_images,
          ls_image              LIKE LINE OF lt_images,
*
          ls_similarity_scoring LIKE LINE OF mt_similarity_scoring.
*
    BREAK-POINT ID zbpid_imago.
    IF is_image IS INITIAL. RETURN. ENDIF.
*
    READ TABLE mt_similarity_scoring INTO ls_similarity_scoring
      WITH KEY source_id = is_image-image->filename( ).
    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE cx_dm_not_found.
    ENDIF.
*
    LOOP AT mt_similarity_scoring INTO ls_similarity_scoring
      WHERE source_id EQ is_image-image->filename( ).
*
      TRY.
          CLEAR ls_image.
          ls_image = get_image( ls_similarity_scoring-dest_id ).
        CATCH cx_dm_not_found.
          CONTINUE.
      ENDTRY.
*
      ls_image-score = ls_similarity_scoring-score.
      APPEND ls_image TO lt_images.
*
    ENDLOOP.
    IF sy-subrc IS NOT INITIAL OR lt_images[] IS INITIAL.
      RAISE EXCEPTION TYPE cx_dm_not_found.
    ENDIF.
*
    rt_images[] = lt_images[].
*
  ENDMETHOD.


  METHOD load_file.
*
    DATA: ls_similarity_scoring LIKE LINE OF mt_similarity_scoring,
*
          ls_image_data         TYPE ztb_gallery,
          ls_image              LIKE LINE OF mt_images,
*
          lo_image              TYPE REF TO zcl_image.
*
*   BREAK-POINT ID zbpid_imago.

* >>>
*    TRY.
*        lv_file_content = zcl_file_services=>read_file( zcl_file_services=>mc_simscore_file  ).
*      CATCH cx_rsl_ui_upload_error.
*        RETURN.
*    ENDTRY.
* >>>

    mt_similarity_scoring = zcl_scp_api=>get_similarity_score( iv_file_content ).

*   Build Image Gallery
    LOOP AT mt_similarity_scoring INTO ls_similarity_scoring GROUP BY ls_similarity_scoring-source_id.
*
      CLEAR ls_image_data.
      SELECT * UP TO 1 ROWS FROM ztb_gallery INTO CORRESPONDING FIELDS OF @ls_image_data
        WHERE filename = @ls_similarity_scoring-source_id ORDER BY valpos DESCENDING.
      ENDSELECT.
      IF sy-subrc IS NOT INITIAL. CONTINUE. ENDIF.
*
      TRY.
          CREATE OBJECT lo_image
            EXPORTING
              is_image_data = ls_image_data.
          IF lo_image IS NOT BOUND. CONTINUE. ENDIF.
        CATCH cx_smime.
          CONTINUE.
      ENDTRY.
*
      MOVE-CORRESPONDING ls_image_data TO ls_image.
*
      ls_image-url = zcl_wda_imago_services=>build_url( iv_xcontent = lo_image->content( )
                                                        iv_mimetype = lo_image->mimetype( )
                                                        iv_ext      = lo_image->fileext( ) ).
*
      ls_image-image = lo_image.
      APPEND ls_image TO mt_images.
*
    ENDLOOP.
*
  ENDMETHOD.
ENDCLASS.
