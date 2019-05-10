class ZCL_SCP_API definition
  public
  create public .

public section.

  types:
    BEGIN OF msty_score,
        source_id TYPE string,
        dest_id   TYPE string,
        score     TYPE decfloat16,
      END OF msty_score .
  types:
    mtty_scores TYPE STANDARD TABLE OF msty_score WITH NON-UNIQUE KEY source_id .
  types:
    BEGIN OF msty_tokens,
        token1 TYPE string,
        token2 TYPE string,
        token3 TYPE string,
        token4 TYPE string,
        token5 TYPE string,
        token6 TYPE string,
        token7 TYPE string,
      END OF msty_tokens .
  types:
    mtty_texts TYPE STANDARD TABLE OF msty_tokens WITH NON-UNIQUE KEY table_line .
  types:
    mtty_options TYPE STANDARD TABLE OF ihttpnvp WITH KEY table_line .

  constants MC_HTTP_OK type I value 200 ##NO_TEXT.
  constants MC_FEATURE_EXTRACTION type STRING value '/ml/featureextraction/inference_sync' ##NO_TEXT.
  constants MC_SIMILARITY_SCORING type STRING value '/ml/similarityscoring/inference_sync' ##NO_TEXT.
  constants MC_OCR type STRING value '/ml/ocr/ocr' ##NO_TEXT.

  methods CLOSE .
  methods CONSTRUCTOR
    importing
      !IV_API_SITE type CLIKE
      !IV_API_KEY type CLIKE optional
      !IV_API type CLIKE optional
    raising
      CX_SY_CREATE_OBJECT_ERROR .
  methods SET_FEATUREEXTRACTION_REQUEST
    importing
      !IO_IMAGE type ref to ZCL_IMAGE .
  methods SET_SIMILARITYSCORING_REQUEST
    importing
      !IV_ZIP_FILE type XSTRING
      !IT_OPTIONS type MTTY_OPTIONS optional .
  methods GET_RESPONSE
    returning
      value(RV_RESPONSE) type STRING
    raising
      CX_RSHDP_HTTP_ERROR .
  class-methods GET_SIMILARITY_SCORE
    importing
      !IV_RESPONSE type STRING
    returning
      value(RT_SCORES) type MTTY_SCORES .
  class-methods GET_FEATURES
    importing
      !IV_RESPONSE type STRING
    returning
      value(RV_RESPONSE) type STRING .
  class-methods GET_OCR_TEXTS
    importing
      !IV_RESPONSE type STRING
    returning
      value(RV_TEXTS) type STRING .
  methods SET_API
    importing
      !IV_API type CLIKE .
  methods SET_OCR_REQUEST
    importing
      !IV_XCONTENT type XSTRING
      !IT_OPTIONS type MTTY_OPTIONS optional .
protected section.
private section.

  class-data MO_HTTP_CLIENT type ref to IF_HTTP_CLIENT .
  class-data MV_API_SITE type STRING .
  class-data MV_API_KEY type STRING .
  class-data MV_API type STRING .
  class-data MV_URL type STRING .

  methods _CONVERT
    importing
      !IO_HTTP_CLIENT type ref to IF_HTTP_CLIENT optional
    returning
      value(RV_RESPONSE) type STRING .
  methods _CLOSE .
ENDCLASS.



CLASS ZCL_SCP_API IMPLEMENTATION.


  METHOD close.
*
    IF mo_http_client IS BOUND.
      _close( ).
      mo_http_client->close( ).
    ENDIF.
*
  ENDMETHOD.


  METHOD constructor.
*
*   BREAK-POINT ID zbpid_scp.
    mv_api_site = iv_api_site.
    mv_api_key  = iv_api_key.
*
    cl_http_client=>create_by_url(  EXPORTING url    = mv_api_site
                                    IMPORTING client = mo_http_client
                                    EXCEPTIONS argument_not_found = 1
                                               plugin_not_active  = 2
                                               internal_error     = 3
                                               OTHERS             = 4 ).
    IF sy-subrc IS NOT INITIAL OR mo_http_client IS NOT BOUND.
      RAISE EXCEPTION TYPE cx_sy_create_object_error.
    ENDIF.
*
    IF iv_api IS SUPPLIED. me->set_api( iv_api ). ENDIF.
*
  ENDMETHOD.


  METHOD get_features.
*
    CONSTANTS lc_comma TYPE c LENGTH 1 VALUE ','.
*
    DATA: ls_featureextraction TYPE lsty_featureextraction,
          lt_feature_vector    TYPE ltty_feature_vector,
          lv_response          TYPE string,
          lv_parsed_response   TYPE string,
          lv_nr_of_features    TYPE i.
*
    BREAK-POINT ID zbpid_scp.
    lv_response = iv_response.
    /ui2/cl_json=>deserialize( EXPORTING json        = lv_response
                               CHANGING  data        = ls_featureextraction ).
*
    lt_feature_vector[] = ls_featureextraction-predictions[ 1 ]-feature_vector.
*
    lv_nr_of_features = lines( lt_feature_vector ).
    IF lv_nr_of_features IS INITIAL. RETURN. ENDIF.
*
    lv_parsed_response = '[' && cl_abap_char_utilities=>cr_lf.
    LOOP AT lt_feature_vector INTO DATA(feature_vector_line).
      lv_parsed_response = lv_parsed_response  &&
                           feature_vector_line.
      IF sy-tabix LT lv_nr_of_features.
        lv_parsed_response = lv_parsed_response && lc_comma.
      ENDIF.
      lv_parsed_response = lv_parsed_response &&
                           cl_abap_char_utilities=>cr_lf.
    ENDLOOP.
*
    lv_parsed_response = lv_parsed_response && ']'.
    rv_response = lv_parsed_response.
*
  ENDMETHOD.


  METHOD get_ocr_texts.
*
    CONSTANTS: lc_new_line TYPE string VALUE '\n',
               lc_tab      TYPE string VALUE '\u'.
*
    DATA lv_response TYPE string.
*
    DATA: ls_ocr_text TYPE lsty_ocr_text,
*
          lt_texts    TYPE ltty_texts,
*
          lv_text     TYPE string,
          lv_line     TYPE string.
*
* >>>
*   lv_response = iv_response.
*   DATA(rr_data) = /ui2/cl_json=>generate( EXPORTING json = lv_response ).
* >>>
*
*   BREAK-POINT ID zbpid_scp.
    lv_response = iv_response.
    /ui2/cl_json=>deserialize( EXPORTING json = lv_response
                               CHANGING  data = ls_ocr_text ).
*
    lt_texts[] =  ls_ocr_text-predictions[].
    IF lt_texts IS INITIAL. RETURN. ENDIF.
*
    LOOP AT lt_texts INTO lv_text.
      lv_line = lv_line &&
                lv_text &&
                cl_abap_char_utilities=>newline.
    ENDLOOP.
    rv_texts = lv_line.
*
  ENDMETHOD.


  METHOD get_response.
*
    DATA: lv_code     TYPE i,
          lv_message  TYPE string,
*
          lv_response TYPE string.
*
    DATA lo_exception TYPE REF TO cx_rshdp_http_error.
*
*   BREAK-POINT ID zbpid_scp.
    TRY.
        mo_http_client->send( EXCEPTIONS http_communication_failure = 1
                                         http_invalid_state         = 2
                                         http_processing_failed     = 3
                                         http_invalid_timeout       = 4
                                         OTHERS                     = 5 ).
        IF sy-subrc IS NOT INITIAL.
          RAISE EXCEPTION TYPE cx_rshdp_http_error.
        ENDIF.
*
        mo_http_client->receive( EXCEPTIONS http_communication_failure = 1
                                            http_invalid_state         = 2
                                            http_processing_failed     = 3
                                            OTHERS                     = 4 ).
        IF sy-subrc IS NOT INITIAL.
          RAISE EXCEPTION TYPE cx_rshdp_http_error.
        ENDIF.
*
        mo_http_client->response->get_status( IMPORTING code   = lv_code
                                                        reason = lv_message ).
        IF lv_code NE mc_http_ok.
          BREAK-POINT ID zbpid_scp.
          RAISE EXCEPTION TYPE cx_rshdp_http_error.
        ENDIF.
*
      CATCH cx_rshdp_http_error INTO lo_exception.
        me->_close( ).
        RAISE EXCEPTION lo_exception.
    ENDTRY.
*
*   BREAK-POINT ID zbpid_scp.
    lv_response = mo_http_client->response->get_cdata( ).
    me->_close( ).
*
    rv_response = lv_response.
*
  ENDMETHOD.


    METHOD get_similarity_score.
*
      DATA: lv_response TYPE string.
*
      DATA: ls_similarityscoring TYPE lsty_similarityscoring,
*
            lt_similarvectors    TYPE ltty_similarvectors,
            ls_similarvector     LIKE LINE OF lt_similarvectors,
*
            lt_vectors           TYPE ltty_vectors,
            ls_vector            LIKE LINE OF lt_vectors,
*
            lt_scores            TYPE mtty_scores,
            ls_score             LIKE LINE OF lt_scores.
*
      BREAK-POINT ID zbpid_scp.
      lv_response = iv_response.
*
      /ui2/cl_json=>deserialize( EXPORTING json        = lv_response
                                 CHANGING  data        = ls_similarityscoring ).
      lt_similarvectors[] = ls_similarityscoring-predictions[].
*
      LOOP AT lt_similarvectors INTO ls_similarvector.
        CLEAR ls_score.
*
        lt_vectors[] = ls_similarvector-similarvectors[].
        ls_score-source_id = zcl_file_services=>decode_filename( ls_similarvector-id ).
        LOOP AT lt_vectors INTO ls_vector.
          ls_score-dest_id = zcl_file_services=>decode_filename( ls_vector-id ).
          ls_score-score   = CONV decfloat16( ls_vector-score ) .
          APPEND ls_score TO lt_scores.
        ENDLOOP.
*
      ENDLOOP.
*
      rt_scores[] = lt_scores[].
*
    ENDMETHOD.


  METHOD set_api.
*
*   BREAK-POINT ID zbpid_scp.
    mv_api = iv_api.
    mv_url = mv_api_site && mv_api.
*
    cl_http_utility=>set_request_uri( request = mo_http_client->request
                                      uri     = mv_api ).
*
  ENDMETHOD.


  METHOD set_featureextraction_request.
* https://archive.sap.com/discussions/thread/3257799
*
    DATA lo_multipart TYPE REF TO if_http_entity.
*
    DATA: lv_form_data TYPE string,
          lv_filename  TYPE string.
*
*   BREAK-POINT ID zbpid_scp.
    me->set_api( zcl_scp_api=>mc_feature_extraction ).
*
    mo_http_client->request->set_method( if_http_request=>co_request_method_post ).
*
    mo_http_client->request->set_header_field( name = 'Accept' value = 'application/json' ).
    mo_http_client->request->set_header_field( name = 'APIKey' value = mv_api_key ).
*
    mo_http_client->request->set_content_type( content_type = 'multipart/form-data' ).
*
    lv_filename = io_image->filename( ).
*
    lv_form_data = |form-data; name="files"; filename="{ lv_filename }"|.
*
    lo_multipart = mo_http_client->request->add_multipart( ).
    lo_multipart->set_header_field(  name  = if_http_header_fields=>content_disposition
                                     value = lv_form_data ).
    lo_multipart->set_data( io_image->content( ) ).
*
  ENDMETHOD.


  METHOD set_ocr_request.
* https://archive.sap.com/discussions/thread/3257799
*
    DATA lo_multipart TYPE REF TO if_http_entity.
*
    DATA: ls_option              LIKE LINE OF it_options,
          lv_form_data           TYPE string,
          lv_content_disposition TYPE string.
*
*   BREAK-POINT ID zbpid_scp.
    me->set_api( zcl_scp_api=>mc_ocr ).
*
    mo_http_client->request->set_method( if_http_request=>co_request_method_post ).
*
    mo_http_client->request->set_header_field( name = 'Accept' value = 'application/json' ).
    mo_http_client->request->set_header_field( name = 'APIKey' value = mv_api_key ).
*
    mo_http_client->request->set_content_type( content_type = 'multipart/form-data' ).
*
    lo_multipart = mo_http_client->request->add_multipart( ).
    lv_content_disposition = |form-data; name="options"|.
    lo_multipart->set_header_field(  name  = if_http_header_fields=>content_disposition
                                     value = lv_content_disposition ).
*
    ls_option-name = 'lang'. ls_option-value = 'en,it'.
    lv_form_data = '{' && | "{ ls_option-name }":"{ ls_option-value }", |.
*
    ls_option-name = 'output_type'. ls_option-value = 'txt'.
    lv_form_data = lv_form_data && | "{ ls_option-name }":"{ ls_option-value }", |.
*
    ls_option-name = 'page_seg_mode'. ls_option-value = '1'.
    lv_form_data = lv_form_data && | "{ ls_option-name }":"{ ls_option-value }", |.
*
    ls_option-name = 'model_type'. ls_option-value = 'lstm_standard'.
    lv_form_data = lv_form_data && | "{ ls_option-name }":"{ ls_option-value }" |.
    lv_form_data = lv_form_data && '}'.
*
    CONDENSE lv_form_data NO-GAPS.
    lo_multipart->set_cdata( lv_form_data ).
*
    lo_multipart = mo_http_client->request->add_multipart( ).
*
    ls_option-name = 'files'. ls_option-value = 'Ticket.JPG'.
    lv_content_disposition = |form-data; name="files"; filename="{ ls_option-value }"|.
    lo_multipart->set_header_field(  name  = if_http_header_fields=>content_disposition
                                     value = lv_content_disposition ).

*   lo_multipart->set_content_type( content_type = 'application/x-zip-compressed').

    lo_multipart->set_data( data   = iv_xcontent
                            length = xstrlen( iv_xcontent ) ).
*
  ENDMETHOD.


  METHOD set_similarityscoring_request.
* https://archive.sap.com/discussions/thread/3257799
*
    DATA lo_multipart TYPE REF TO if_http_entity.
*
    DATA: ls_option              LIKE LINE OF it_options,
          lv_form_data           TYPE string,
          lv_content_disposition TYPE string.
*
*   BREAK-POINT ID zbpid_scp.
    me->set_api( zcl_scp_api=>mc_similarity_scoring ).
*
    mo_http_client->request->set_method( if_http_request=>co_request_method_post ).
*
    mo_http_client->request->set_header_field( name = 'Accept' value = 'application/json' ).
    mo_http_client->request->set_header_field( name = 'APIKey' value = mv_api_key ).
*
    mo_http_client->request->set_content_type( content_type = 'multipart/form-data' ).
*
    lo_multipart = mo_http_client->request->add_multipart( ).
    lv_content_disposition = |form-data; name="options"|.
    lo_multipart->set_header_field(  name  = if_http_header_fields=>content_disposition
                                     value = lv_content_disposition ).
*
    READ TABLE it_options INTO ls_option WITH KEY name = 'numSimilarVectors'. "Must Be!
    lv_form_data = '{' && |"{ ls_option-name }":{ ls_option-value }|.
*
    READ TABLE it_options INTO ls_option WITH KEY name = 'algorithm'.
    IF sy-subrc IS INITIAL AND ls_option-value IS NOT INITIAL.
      lv_form_data = lv_form_data && |, "{ ls_option-name }":"{ ls_option-value }"|.
    ENDIF.
    lv_form_data = lv_form_data && '}'. CONDENSE lv_form_data NO-GAPS.
    lo_multipart->set_cdata( lv_form_data ).
*
    lo_multipart = mo_http_client->request->add_multipart( ).
*
    READ TABLE it_options INTO ls_option WITH KEY name = 'fileName'. "Must-be
    lv_content_disposition = |form-data; name="files"; filename="{ ls_option-value }"|.
    lo_multipart->set_header_field(  name  = if_http_header_fields=>content_disposition
                                     value = lv_content_disposition ).
    lo_multipart->set_content_type( content_type = 'application/x-zip-compressed').
    lo_multipart->set_data( data   = iv_zip_file
                            length = xstrlen( iv_zip_file ) ).
*
  ENDMETHOD.


  METHOD _close.
    mo_http_client->refresh_request( ).
  ENDMETHOD.


  METHOD _convert.
*
    DATA lo_http_client TYPE REF TO if_http_client.
    DATA lv_xresponse TYPE xstring.
*
* >>>
*   lv_line = escape( val    = lv_line
*                     format = cl_abap_format=>e_json_string ).
* >>>
*
*   BREAK-POINT ID zbpid_scp.
    lo_http_client = mo_http_client.
    IF io_http_client IS SUPPLIED AND io_http_client IS BOUND.
      lo_http_client = io_http_client.
    ENDIF.
    IF lo_http_client IS NOT BOUND. RETURN. ENDIF.
*
    lv_xresponse = lo_http_client->response->get_data( ).
    rv_response = /ui2/cl_json=>raw_to_string( EXPORTING iv_xstring  = lv_xresponse ).
"                                                        iv_encoding = lv_encoding ).
*
  ENDMETHOD.
ENDCLASS.
