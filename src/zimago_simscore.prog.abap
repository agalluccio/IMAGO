*&---------------------------------------------------------------------*
*& Report ZIMAGO_SIMSCORE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zimago_simscore.
*
DATA: gv_response TYPE string,
      gt_scores   TYPE zcl_scp_api=>mtty_scores.
*
DATA: gv_spath      TYPE rlgrap-filename,
      gt_dynpfields TYPE STANDARD TABLE OF dynpread,
      gs_dynpfield  LIKE LINE OF gt_dynpfields.
*
SELECTION-SCREEN BEGIN OF BLOCK bl0 WITH FRAME TITLE TEXT-000.
SELECTION-SCREEN SKIP 1.
PARAMETERS: p_path TYPE rlgrap-filename OBLIGATORY.
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN END OF BLOCK bl0.
*
INITIALIZATION.
  gv_spath =
  p_path   = zcl_file_services=>mc_simscore_file.
*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path.
  gv_spath =
  p_path   = zcl_file_services=>file_open_dialog( gv_spath ).
*
START-OF-SELECTION.
* BREAK-POINT ID zbpid_imago.
  TRY.
      gv_response = zcl_file_services=>read_file( gv_spath ).
*
      gt_scores = zcl_scp_api=>get_similarity_score( gv_response ).
*
    CATCH cx_rsl_ui_upload_error.
      MESSAGE i398(00) WITH 'Unable to upload file'. RETURN.
*
  ENDTRY.
*
END-OF-SELECTION.
*
