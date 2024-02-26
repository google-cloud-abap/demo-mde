*&---------------------------------------------------------------------*
*& Include          ZGOOG_CREATE_MAINT_ORDER_IMPL
*&---------------------------------------------------------------------*

CLASS lcl_main IMPLEMENTATION.

  METHOD execute.

    CALL METHOD pull_pubsub_messages
      IMPORTING
        ex_t_messages = DATA(lt_messages).
    IF lt_messages IS NOT INITIAL.
      CALL METHOD create_maintenance_order
        EXPORTING
          im_t_messages = lt_messages
        IMPORTING
          ex_t_output   = DATA(lt_output).
      IF lt_output IS NOT INITIAL.
        CALL METHOD display_output
          EXPORTING
            im_t_output = lt_output.

      ENDIF.
    ELSE.
      MESSAGE 'No maintenance request available for processing' TYPE mc_s DISPLAY LIKE mc_e.
      RETURN.

    ENDIF.

  ENDMETHOD.

  METHOD pull_pubsub_messages.

    DATA:
      lv_p_projects_id      TYPE string,
      lv_p_subscriptions_id TYPE string,
      lv_msg                TYPE string,
      ls_input              TYPE /goog/cl_pubsub_v1=>ty_026,
      ls_message            TYPE /goog/cl_pubsub_v1=>ty_029,
      ls_input_ack          TYPE /goog/cl_pubsub_v1=>ty_001.

    TRY.
* Open HTTP Connection
        DATA(lo_client) = NEW /goog/cl_pubsub_v1( iv_key_name = p_key ).

* Populate relevant parameters
        lv_p_projects_id = lo_client->gv_project_id.
        lv_p_subscriptions_id = p_subsid. "'create-maintenance-orders-sub'.
        ls_input-max_messages = 50.

* Call API method
        CALL METHOD lo_client->pull_subscriptions
          EXPORTING
            iv_p_projects_id      = lv_p_projects_id
            iv_p_subscriptions_id = lv_p_subscriptions_id
            is_input              = ls_input
          IMPORTING
*           es_raw                =
            es_output             = DATA(ls_output)
            ev_ret_code           = DATA(lv_ret_code)
            ev_err_text           = DATA(lv_err_text)
            es_err_resp           = DATA(ls_err_resp).
        IF lo_client->is_success( lv_ret_code ).
          IF ls_output-received_messages IS NOT INITIAL.
            ls_input_ack-ack_ids = VALUE #( FOR ls_msg IN ls_output-received_messages ( ls_msg-ack_id ) ).
            CALL METHOD lo_client->acknowledge_subscriptions
              EXPORTING
                iv_p_projects_id      = lv_p_projects_id
                iv_p_subscriptions_id = lv_p_subscriptions_id
                is_input              = ls_input_ack
              IMPORTING
                ev_ret_code           = lv_ret_code
                ev_err_text           = lv_err_text
                es_err_resp           = ls_err_resp.

            LOOP AT ls_output-received_messages INTO ls_message.
              lv_msg = cl_http_utility=>decode_base64( encoded = ls_message-message-data ).
              APPEND lv_msg TO ex_t_messages.

            ENDLOOP.

          ENDIF.

        ENDIF.

* Close HTTP Connection
        lo_client->close( ).

      CATCH /goog/cx_sdk INTO DATA(lo_exception).
        MESSAGE lo_exception->get_text( ) TYPE mc_e.

    ENDTRY.

  ENDMETHOD.

  METHOD create_maintenance_order.

    TYPES:
      BEGIN OF lty_order_data,
        order_type       TYPE string,
        short_text       TYPE string,
        equipment_number TYPE string,
        work_center      TYPE string,

      END OF lty_order_data.

*      BEGIN OF lty_maintenance,
*        id               TYPE string,
*        schema_id        TYPE string,
*        provider         TYPE string,
*        bucket_name      TYPE string,
*        bucket_version   TYPE string,
*        version          TYPE string,
*        attributes       TYPE lty_order_data,
*        schema_entry_ref TYPE string,
*        provider_id      TYPE string,
*
*      END OF lty_maintenance.
*
*    TYPES:
*          ltt_maintenance TYPE STANDARD TABLE OF lty_maintenance.

    DATA:
      lt_return     TYPE STANDARD TABLE OF bapiret2,
      lt_msg        TYPE bal_t_msg,
      lt_caufvdn    TYPE STANDARD TABLE OF caufvdn,
      lt_messages   TYPE TABLE OF string,
      lt_order_data TYPE STANDARD TABLE OF lty_order_data.
*      lt_registry   TYPE ltt_maintenance.

    DATA:
      ls_header       TYPE alm_me_order_header,
      ls_order_header TYPE alm_me_order_header,
      ls_return       TYPE bapiret2,
      ls_caufvdn      TYPE caufvdn,
      ls_general_data TYPE bapi_itob,
      ls_order_data   TYPE lty_order_data,
      ls_output       TYPE mty_output.
*      ls_maintenance  TYPE lty_maintenance.

    DATA:
          lv_equnr TYPE equnr.

    DATA:
      lo_def              TYPE REF TO data,
      lo_maintenance      TYPE REF TO data,
      lo_data_access      TYPE REF TO /ui2/cl_data_access,
      lo_line_data_access TYPE REF TO /ui2/cl_data_access,
      lo_line_field       TYPE REF TO data.

    LOOP AT im_t_messages ASSIGNING FIELD-SYMBOL(<ls_messages>).
*      /goog/cl_json_util=>deserialize_json( EXPORTING iv_json        = <ls_messages>
*                                                      iv_pretty_name = /ui2/cl_json=>pretty_mode-extended
*                                            IMPORTING es_data        = ls_order_data ).
*      APPEND ls_order_data TO lt_order_data.

      /goog/cl_gcp_json_util=>deserialize_json( EXPORTING iv_json = <ls_messages> IMPORTING es_data = lo_def ).
      lo_data_access = /ui2/cl_data_access=>create( ir_data = lo_def ).

*      lo_data_access->at( |TAG-METADATA-REGISTRY-SAP_ABAP_MAINTENANCE_2| )->ref(  ).( IMPORTING ev_data = lt_registry ).
*      lo_maintenance = lo_data_access->at( |TAG-METADATA-REGISTRY-SAP_ABAP_MAINTENANCE_2| )->ref( ).
*
*      LOOP AT lo_maintenance->* ASSIGNING FIELD-SYMBOL(<ls_maintenance>).
*        lo_line_data_access = /ui2/cl_data_access=>create( ir_data = <ls_maintenance> ).
*        lo_line_field = lo_line_data_access->at( |ATTRIBUTES-ORDER_TYPE| )->ref( ).
*        ls_order_data-order_type = lo_line_field->*.
*
*        lo_line_field = lo_line_data_access->at( |ATTRIBUTES-SHORT_TEXT| )->ref( ).
*        ls_order_data-short_text = lo_line_field->*.
*
*        lo_line_field = lo_line_data_access->at( |ATTRIBUTES-EQUIPMENT_NUMBER| )->ref( ).
*        ls_order_data-equipment_number = lo_line_field->*.
*
*        lo_line_field = lo_line_data_access->at( |ATTRIBUTES-WORK_CENTER| )->ref( ).
*        ls_order_data-work_center = lo_line_field->*.
*
*      ENDLOOP.

      lo_line_field = lo_data_access->at( |ORDER_TYPE| )->ref( ).
      ls_order_data-order_type = lo_line_field->*.

      lo_line_field = lo_data_access->at( |SHORT_TEXT| )->ref( ).
      ls_order_data-short_text = lo_line_field->*.

      lo_line_field = lo_data_access->at( |EQUIPMENT_NUMBER| )->ref( ).
      ls_order_data-equipment_number = lo_line_field->*.

      lo_line_field = lo_data_access->at( |WORK_CENTER| )->ref( ).
      ls_order_data-work_center = lo_line_field->*.

      APPEND ls_order_data TO lt_order_data.

    ENDLOOP.

    LOOP AT lt_order_data ASSIGNING FIELD-SYMBOL(<ls_order_data>).
      lv_equnr = <ls_order_data>-equipment_number.
      lv_equnr = |{ lv_equnr ALPHA = IN } |.
      CALL FUNCTION 'BAPI_EQUI_GETDETAIL'
        EXPORTING
          equipment        = lv_equnr
        IMPORTING
          data_general_exp = ls_general_data
          return           = ls_return.

      ls_header-order_type = <ls_order_data>-order_type.
      SELECT SINGLE objid
        FROM crtx
        INTO @DATA(lv_objid)
        WHERE objty = 'A' AND
              ktext = @<ls_order_data>-work_center.
      IF sy-subrc = 0.
        SELECT SINGLE arbpl
          FROM crhd
          INTO @DATA(lv_arbpl)
          WHERE objid = @lv_objid.
        IF sy-subrc = 0.
          ls_header-mn_wk_ctr = lv_arbpl.
        ELSE.
          ls_header-mn_wk_ctr  = <ls_order_data>-work_center.

        ENDIF.
      ELSE.
        ls_header-mn_wk_ctr  = <ls_order_data>-work_center.

      ENDIF.
      ls_header-equipment  = lv_equnr.
      ls_header-plant      = ls_general_data-planplant.
      ls_header-short_text = <ls_order_data>-short_text.
      ls_header-start_date = sy-datum.
      ls_header-finish_date = sy-datum + 2.

      CALL FUNCTION 'ALM_ME_ORDER_CREATE'
        EXPORTING
          order_header      = ls_header
          i_partner_tpa_key = 'X'
        IMPORTING
          e_order_header    = ls_order_header
        TABLES
          return            = lt_return.
      READ TABLE lt_return INTO ls_return
        WITH KEY type = 'E'.
      IF sy-subrc NE 0.
        CALL FUNCTION 'ALM_ME_ORD_BAPI_ORDER_POST'
          TABLES
            et_messages         = lt_msg
            et_caufvdn          = lt_caufvdn
          EXCEPTIONS
            message_without_log = 1
            OTHERS              = 2.
        IF sy-subrc EQ 0.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.
          READ TABLE lt_caufvdn INTO ls_caufvdn INDEX 1.
          IF sy-subrc EQ 0.
            ls_output-aufart  = <ls_order_data>-order_type.
            ls_output-equnr   = <ls_order_data>-equipment_number.
            ls_output-gewrk   = <ls_order_data>-work_center.
            ls_output-remarks = 'Order created successfully'.
            ls_output-aufnr   = ls_caufvdn-aufnr_neu.

            APPEND ls_output TO ex_t_output.
            CLEAR ls_output.

          ENDIF.

        ENDIF.
      ELSE.
        ls_output-aufart  = <ls_order_data>-order_type.
        ls_output-equnr   = <ls_order_data>-equipment_number.
        ls_output-gewrk   = <ls_order_data>-work_center.
        ls_output-remarks = 'Error while creating Order'.

        APPEND ls_output TO ex_t_output.
        CLEAR ls_output.

      ENDIF.

      CLEAR:
            ls_return,
            lt_return.

    ENDLOOP.

  ENDMETHOD.

  METHOD display_output.

    DATA:
          lo_column     TYPE REF TO cl_salv_column_table.

    DATA(lt_output) = im_t_output.

    TRY.
        CALL METHOD cl_salv_table=>factory
          IMPORTING
            r_salv_table = DATA(lo_alv)
          CHANGING
            t_table      = lt_output.
      CATCH cx_salv_msg.
    ENDTRY.

    DATA(lo_functions) = lo_alv->get_functions( ).
    lo_functions->set_all( abap_true ).

    DATA(lo_columns) = lo_alv->get_columns( ).
    lo_columns->set_optimize( 'X' ).

    TRY.
        lo_column ?= lo_columns->get_column( 'REMARKS' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Remarks' ).
        lo_column->set_medium_text( 'Remarks' ).
        lo_column->set_short_text( 'Remarks' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    lo_alv->display( ).

  ENDMETHOD.

ENDCLASS.
