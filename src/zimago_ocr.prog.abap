*&---------------------------------------------------------------------*
*& Report ZIMAGO_OCR
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zimago_ocr.
*
TYPES: gtty_images     TYPE STANDARD TABLE OF REF TO zcl_image,
       gtty_files      TYPE STANDARD TABLE OF file_info WITH EMPTY KEY,
*
       gty_pict_line   TYPE x LENGTH 1022,
       gtty_pict_lines TYPE STANDARD TABLE OF gty_pict_line.
*
DATA: gt_files  TYPE gtty_files,
      gt_images TYPE gtty_images,
*
      gv_texts  TYPE string.
*
DATA: go_image       TYPE LINE OF gtty_images,
      go_exception   TYPE REF TO cx_root,
      go_api_manager TYPE REF TO zcl_scp_api.
*
DATA: gv_response TYPE string,
      gv_url      TYPE string.
*
DATA: gv_ok_code TYPE okcode.
*
DATA: go_image_cont TYPE REF TO cl_gui_custom_container,
      go_text_cont  TYPE REF TO cl_gui_custom_container,
*
      go_picture    TYPE REF TO cl_gui_picture,
      go_textedit   TYPE REF TO cl_gui_textedit.
*
SELECTION-SCREEN BEGIN OF BLOCK bl0 WITH FRAME TITLE TEXT-000.
SELECTION-SCREEN SKIP 1.
PARAMETERS: p_path TYPE rlgrap-filename OBLIGATORY DEFAULT 'K:\Temp\ImageGallery\Ocr\'.
SELECTION-SCREEN SKIP 1.
PARAMETERS: scp_api TYPE rlgrap-filename DEFAULT 'https://sandbox.api.sap.com' OBLIGATORY,
            api_key TYPE rlgrap-filename DEFAULT 'VTsOkDAMCHhVQV1NkZ7xuSOms9JvORLS' OBLIGATORY.
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN END OF BLOCK bl0.
*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path.
  p_path = zcl_file_services=>get_dir( p_path ).
*
START-OF-SELECTION.
*
* BREAK-POINT ID zbpid_imago.
  TRY.
*
      zcl_file_services=>get_files( EXPORTING  iv_gallery_directory = p_path
                                    IMPORTING  et_files             = gt_files
                                    EXCEPTIONS no_files_founds      = 1
                                               OTHERS               = 2 ).
      IF sy-subrc IS NOT INITIAL OR gt_files[] IS INITIAL.
        RAISE EXCEPTION TYPE cx_list_error_empty_list.
      ENDIF.
*
      LOOP AT gt_files ASSIGNING FIELD-SYMBOL(<fs_image>).
*
        TRY.
            CREATE OBJECT go_image
              EXPORTING
                iv_filename = <fs_image>-filename.
            APPEND go_image TO gt_images.
*
          CATCH cx_smime.
            CONTINUE.
        ENDTRY.
*
      ENDLOOP.
      IF gt_images[] IS INITIAL. RAISE EXCEPTION TYPE cx_list_error_empty_list. ENDIF.
*
      CREATE OBJECT go_api_manager
        EXPORTING
          iv_api_site = scp_api
          iv_api_key  = api_key.
*
      LOOP AT gt_images INTO go_image.
*
        TRY.
*
            go_api_manager->set_ocr_request( go_image->content( )  ).
            gv_response = go_api_manager->get_response( ).
            gv_texts = zcl_scp_api=>get_ocr_texts( gv_response ).
*
          CATCH cx_rshdp_http_error.
            CONTINUE.
        ENDTRY.
*
      ENDLOOP.
*
      CALL SCREEN 100.
*
    CATCH cx_list_error_empty_list.
      MESSAGE i398(00) WITH 'No files Founds'. RETURN.
*
    CATCH cx_sy_create_object_error.
      MESSAGE i398(00) WITH 'Unable to Start API Service'. RETURN.
*
  ENDTRY.
*
END-OF-SELECTION.
*
*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
*
  SET PF-STATUS 'STATUS_100'.
  SET TITLEBAR '100'.
*
  IF go_image_cont IS INITIAL.
    PERFORM init_picture.
    PERFORM init_textedit.
  ENDIF.
*
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE gv_ok_code.
    WHEN 'BACK' OR 'EXIT' OR 'CANC'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.

FORM init_picture.
*
  CREATE OBJECT go_image_cont
    EXPORTING
      container_name = 'CTRL_IMAGE'.
*
  CREATE OBJECT go_picture
    EXPORTING
      parent = go_image_cont.
*
  go_picture->load_picture_from_url( EXPORTING  url = zcl_image=>build_picture_url( EXPORTING iv_xcontent = go_image->content( )
                                                                                              iv_mimetype = go_image->mimetype( ) )
                                     EXCEPTIONS error  = 1
                                     OTHERS            = 2 ).
  go_picture->set_display_mode( cl_gui_picture=>display_mode_fit ).
*
ENDFORM.

FORM init_textedit.
*
  CREATE OBJECT go_text_cont
    EXPORTING
      container_name = 'CTRL_TEXTS'.
  CREATE OBJECT go_textedit
    EXPORTING
      parent = go_text_cont.
*
  go_textedit->set_readonly_mode( 1 ).
  go_textedit->set_toolbar_mode( 0 ).
  go_textedit->set_statusbar_mode( 0 ).
  go_textedit->set_font_fixed( 1 ).
*
  go_textedit->set_wordwrap_behavior( wordwrap_mode = go_textedit->wordwrap_off  ).
  go_textedit->set_textstream( text = gv_texts ).
*

ENDFORM.
