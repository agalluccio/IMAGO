*&---------------------------------------------------------------------*
*& Report ZIMAGO
*&---------------------------------------------------------------------*
REPORT zimago.
*
CLASS gcl_imago_manager DEFINITION.
*
  PUBLIC SECTION.
*
    TYPES mtty_images TYPE STANDARD TABLE OF REF TO zcl_image.
*
    CLASS-DATA: mo_exception   TYPE REF TO cx_root,
                mo_api_manager TYPE REF TO zcl_scp_api.
*
    METHODS:
*
      set_gallery_source IMPORTING iv_from_gallery TYPE clike,
      store_gallery      IMPORTING iv_store_gallery TYPE clike
                         RAISING   cx_rsdrc_no_commit,
*
      release,
*
      build_gallery IMPORTING iv_gallery_directory TYPE clike OPTIONAL
                    EXPORTING et_images            TYPE mtty_images
                    RAISING   cx_list_error_empty_list
                              cx_rsdrc_no_commit,
*
      get_options   IMPORTING iv_numsimilarvectors TYPE numeric
                              iv_algorithm         TYPE clike   OPTIONAL
                              iv_filename          TYPE clike   OPTIONAL
                    RETURNING VALUE(rt_options)    TYPE zcl_scp_api=>mtty_options.

  PRIVATE SECTION.
    CLASS-DATA: mt_images       TYPE mtty_images,
                mt_gallery      TYPE STANDARD TABLE OF ztb_gallery,
*
                ms_image_data   TYPE ztb_gallery,
                mv_from_gallery TYPE boolean.
*
    METHODS _store_gallery RAISING cx_rsdrc_no_commit.
*
ENDCLASS.
*
CLASS gcl_imago_manager IMPLEMENTATION.
*
  METHOD release.
    mo_api_manager->close( ).
    FREE: mo_api_manager, mt_images, mt_gallery.
  ENDMETHOD.
*
  METHOD store_gallery.
    IF iv_store_gallery EQ abap_false. RETURN. ENDIF.
    _store_gallery( ).
  ENDMETHOD.
*
  METHOD set_gallery_source.
    mv_from_gallery = iv_from_gallery.
  ENDMETHOD.
*
  METHOD _store_gallery.
*
*   BREAK-POINT ID zbpid_imago.
    LOOP AT mt_images ASSIGNING FIELD-SYMBOL(<fo_image>).
      CLEAR ms_image_data.
      ms_image_data-filename    = <fo_image>->filename( ).
      ms_image_data-valpos      = <fo_image>->valpos( iv_for_update = abap_true ).
      ms_image_data-mimetype    = <fo_image>->mimetype( ).
      ms_image_data-filesize    = <fo_image>->size( ).
      ms_image_data-md5_hash    = <fo_image>->md5( ).
      ms_image_data-features    = <fo_image>->features( ).
      ms_image_data-filecontent = <fo_image>->content( ).
      APPEND ms_image_data TO mt_gallery.
    ENDLOOP.
*
    MODIFY ztb_gallery FROM TABLE @mt_gallery.
    IF sy-subrc IS INITIAL AND sy-dbcnt EQ lines( mt_gallery ).
      COMMIT WORK AND WAIT.
    ELSE.
      ROLLBACK WORK.
      RAISE EXCEPTION TYPE cx_rsdrc_no_commit.
    ENDIF.
*
  ENDMETHOD.
*
  METHOD get_options.
*
    DATA ls_option LIKE LINE OF rt_options.
*
    ls_option-name  = 'numSimilarVectors'.
    ls_option-value = iv_numsimilarvectors.
    APPEND ls_option TO rt_options.
*
    CLEAR ls_option.
    ls_option-name  = 'algorithm'. " "naive", "matrix_mult","clustering"
    IF iv_algorithm IS SUPPLIED.
      ls_option-value = iv_algorithm.
      APPEND ls_option TO rt_options.
    ENDIF.
*
    CLEAR ls_option.
    ls_option-name  = 'fileName'.
    ls_option-value = 'VectorSet.zip'.
    IF iv_filename IS SUPPLIED.
      ls_option-value = iv_filename.
    ENDIF.
    APPEND ls_option TO rt_options.
*
  ENDMETHOD.
*
  METHOD build_gallery.
*
    DATA lt_files TYPE zcl_file_services=>mtty_files.
    DATA lo_image TYPE LINE OF mtty_images.
*
*   BREAK-POINT ID zbpid_imago.
    CASE mv_from_gallery.
*
      WHEN abap_true.
        zcl_file_services=>get_files( EXPORTING  iv_gallery_directory = iv_gallery_directory
                                      IMPORTING  et_files             = lt_files
                                      EXCEPTIONS no_files_founds      = 1
                                                 OTHERS               = 2 ).
        IF sy-subrc IS NOT INITIAL.  RAISE EXCEPTION TYPE cx_list_error_empty_list. ENDIF.
*
        LOOP AT lt_files ASSIGNING FIELD-SYMBOL(<fs_file>).
*
          TRY.
              CREATE OBJECT lo_image
                EXPORTING
                  iv_filename = <fs_file>-filename.
              APPEND lo_image TO mt_images.
*
            CATCH cx_smime INTO mo_exception.
              CONTINUE.
          ENDTRY.
*
        ENDLOOP.
        IF mt_images[] IS INITIAL. RAISE EXCEPTION TYPE cx_list_error_empty_list. ENDIF.
*
      WHEN abap_false.
*
        SELECT * FROM ztb_gallery INTO TABLE @mt_gallery. "Avoid Cursor Lost
        IF sy-subrc IS NOT INITIAL. RAISE EXCEPTION TYPE cx_list_error_empty_list. ENDIF.
*
        LOOP AT mt_gallery INTO ms_image_data.
          TRY.
              CREATE OBJECT lo_image
                EXPORTING
                  is_image_data = ms_image_data.
              APPEND lo_image TO mt_images.
*
            CATCH cx_smime INTO mo_exception.
              CONTINUE.
          ENDTRY.
*
        ENDLOOP.
        IF mt_images[] IS INITIAL. RAISE EXCEPTION TYPE cx_list_error_empty_list. ENDIF.
*
    ENDCASE.
*
    et_images[] = mt_images[].
*
  ENDMETHOD.
*
ENDCLASS.
*
DATA go_manager TYPE REF TO gcl_imago_manager.
*
DATA: gt_images TYPE gcl_imago_manager=>mtty_images,
      go_image  LIKE LINE OF gt_images.
*
DATA: gv_response TYPE string,
      gt_scores  TYPE zcl_scp_api=>mtty_scores.
*
SELECTION-SCREEN BEGIN OF BLOCK bl0 WITH FRAME TITLE TEXT-000.
SELECTION-SCREEN SKIP 1.
PARAMETERS: p_path TYPE rlgrap-filename DEFAULT 'K:\Temp\ImageGallery\' OBLIGATORY.
SELECTION-SCREEN SKIP 1.
PARAMETERS: scp_api TYPE rlgrap-filename DEFAULT 'https://sandbox.api.sap.com' OBLIGATORY,
            api_key TYPE rlgrap-filename DEFAULT 'VTsOkDAMCHhVQV1NkZ7xuSOms9JvORLS' OBLIGATORY.
SELECTION-SCREEN SKIP 1.
PARAMETERS: r_frmgal RADIOBUTTON GROUP gr1,
            r_frmdic RADIOBUTTON GROUP gr1.
SELECTION-SCREEN SKIP 1.
PARAMETERS: p_store  AS CHECKBOX,
            psvscore AS CHECKBOX.
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN END OF BLOCK bl0.
*
INITIALIZATION.
  r_frmdic =
  psvscore = abap_true.
*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path.
  p_path = zcl_file_services=>get_dir( p_path ).
*
START-OF-SELECTION.
*
* BREAK-POINT ID zbpid_imago.
  TRY.
*
      CREATE OBJECT go_manager.
      go_manager->set_gallery_source( r_frmgal ).
      go_manager->build_gallery( EXPORTING iv_gallery_directory = p_path
                                 IMPORTING et_images            = gt_images ).
*
    CATCH cx_list_error_empty_list INTO gcl_imago_manager=>mo_exception.
      MESSAGE i398(00) WITH 'No Image Founds'.
      go_manager->release( ). RETURN.
*
  ENDTRY.
*
  TRY.
*
      CREATE OBJECT go_manager->mo_api_manager
        EXPORTING
          iv_api_site = scp_api
          iv_api_key  = api_key.
*
*     Features Extraction API
      LOOP AT gt_images INTO go_image.
*
        CHECK go_image->features( ) IS INITIAL.
        TRY.
*
            go_manager->mo_api_manager->set_featureextraction_request( go_image  ).
            gv_response = go_manager->mo_api_manager->get_response( ).
*
            go_image->set_features( gv_response ).
*
          CATCH cx_rshdp_http_error INTO gcl_imago_manager=>mo_exception.
            CONTINUE.
        ENDTRY.
*
      ENDLOOP.
*
*     Store gallery if requested
      TRY.
          go_manager->store_gallery( p_store ).
*
        CATCH cx_rsdrc_no_commit INTO gcl_imago_manager=>mo_exception.
          MESSAGE i398(00) WITH 'Error during Gallery Store'.
*
      ENDTRY.
*
*     Prepare the .zip file for other API
      TRY.
          zcl_file_services=>create_zip( ).
          LOOP AT gt_images INTO go_image.
            CHECK go_image->is_valid( ) EQ abap_true.
            TRY.
                zcl_file_services=>add_to_zip( go_image ).
              CATCH cx_conversion_failed INTO gcl_imago_manager=>mo_exception.
                CONTINUE.
            ENDTRY.
          ENDLOOP.
          zcl_file_services=>close_zip( ).
*
        CATCH cx_sy_create_object_error INTO gcl_imago_manager=>mo_exception.
          MESSAGE i398(00) WITH 'Unable to Create .Zip file'.
          go_manager->release( ). RETURN.
      ENDTRY.
*
*     Similarity Score API
      go_manager->mo_api_manager->set_similarityscoring_request(
        EXPORTING iv_zip_file = zcl_file_services=>get_zip( )
                  it_options  = go_manager->get_options( iv_numsimilarvectors = zcl_file_services=>count_files_in_zip( ) - 1 ) ).
*
      gv_response = go_manager->mo_api_manager->get_response( ).

*     Store Similarity Score as file if requested
      TRY.
          IF psvscore EQ abap_true.
            zcl_file_services=>save_file( EXPORTING iv_filename = zcl_file_services=>mc_simscore_file
                                                    iv_content = gv_response ).
          ENDIF.
        CATCH cx_tpda_file_download INTO gcl_imago_manager=>mo_exception.
          MESSAGE i398(00) WITH 'Unable to Save Similarity Score file'.
      ENDTRY.
*
      gt_scores  = zcl_scp_api=>get_similarity_score( gv_response ).
*
      go_manager->release( ).
*
    CATCH cx_sy_create_object_error INTO gcl_imago_manager=>mo_exception.
      MESSAGE i398(00) WITH 'Unable to Start API Service'.
      go_manager->release( ). RETURN.
*
    CATCH cx_rshdp_http_error INTO gcl_imago_manager=>mo_exception.
      MESSAGE i398(00) WITH 'Error in call API'.
      go_manager->release( ). RETURN.
*
  ENDTRY.

END-OF-SELECTION.
*
