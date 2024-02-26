FUNCTION zgoog_publish_pm_order_status.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(EVENT) LIKE  SWETYPECOU-EVENT
*"     VALUE(RECTYPE) LIKE  SWETYPECOU-RECTYPE
*"     VALUE(OBJTYPE) LIKE  SWETYPECOU-OBJTYPE
*"     VALUE(OBJKEY) LIKE  SWEINSTCOU-OBJKEY
*"     VALUE(EXCEPTIONS_ALLOWED) LIKE  SWEFLAGS-EXC_OK DEFAULT SPACE
*"  EXPORTING
*"     VALUE(REC_ID) LIKE  SWELOG-RECID
*"  TABLES
*"      EVENT_CONTAINER STRUCTURE  SWCONT
*"  EXCEPTIONS
*"      TEMP_ERROR
*"      ANY_ERROR
*"----------------------------------------------------------------------

  TYPES:
    BEGIN OF lty_value,
      timestamp             TYPE string,  "timestamp,
      order_number          TYPE string,
      order_description     TYPE string,
      order_date            TYPE string,
      order_type            TYPE string,
      equipment_number      TYPE string,
      equipment_description TYPE string,
      maintenance_plant     TYPE string,
      work_center           TYPE string,
      notification_number   TYPE string,
      planned_start_date    TYPE string,
      planned_end_date      TYPE string,
      current_status        TYPE string,
      status_change_date    TYPE string,

    END OF lty_value,

    BEGIN OF lty_publish_message,
      tag_name  TYPE string,
      timestamp TYPE string,  "timestamp,
      value     TYPE lty_value,

    END OF lty_publish_message,

    BEGIN OF lty_publish_message_new,
      tag_name  TYPE string,
      timestamp TYPE string,  "timestamp,
      type      TYPE string,
      value     TYPE lty_value,

    END OF lty_publish_message_new.

  DATA:
    ls_value               TYPE lty_value,
    ls_publish_message     TYPE lty_publish_message,
    ls_publish_message_new TYPE lty_publish_message_new.

  DATA: lv_msg           TYPE string,
        lo_exception     TYPE REF TO /goog/cx_sdk,
        lo_client        TYPE REF TO /goog/cl_pubsub_v1,
        lv_p_projects_id TYPE string,
        ls_message       TYPE /goog/cl_pubsub_v1=>ty_025,
        ls_input         TYPE /goog/cl_pubsub_v1=>ty_023,
        ls_output        TYPE /goog/cl_pubsub_v1=>ty_024,
        lv_ret_code      TYPE i,
        lv_err_text      TYPE string,
        ls_err_resp      TYPE /goog/err_resp,
        lv_timestamp     TYPE timestamp.

  SELECT SINGLE *
    FROM aufk
    INTO @DATA(ls_aufk)
    WHERE aufnr EQ @objkey.
  IF sy-subrc EQ 0.
    SELECT SINGLE *
      FROM afih
      INTO @DATA(ls_afih)
      WHERE aufnr EQ @ls_aufk-aufnr.
    IF sy-subrc EQ 0.
      SELECT SINGLE *
        FROM qmel
        INTO @DATA(ls_qmel)
        WHERE qmnum EQ @ls_afih-qmnum.
      IF sy-subrc EQ 0.
        GET TIME STAMP FIELD lv_timestamp.  "ls_value-timestamp.
*        CONVERT TIME STAMP ls_value-timestamp TIME ZONE 'UTC' INTO DATE DATA(lv_date) TIME DATA(lv_time).
        CONVERT TIME STAMP lv_timestamp TIME ZONE 'UTC' INTO DATE DATA(lv_date) TIME DATA(lv_time).

        cl_pco_utility=>convert_abap_timestamp_to_java(
          EXPORTING
            iv_date      = lv_date
            iv_time      = lv_time
            iv_msec      = 0
          IMPORTING
            ev_timestamp = DATA(lv_unix_iat)
        ).
        ls_value-timestamp = lv_unix_iat.
        ls_value-order_number = ls_aufk-aufnr.
        ls_value-order_description = ls_aufk-ktext.
        ls_value-order_date = ls_aufk-erdat+6(2) && '.' && ls_aufk-erdat+4(2) && '.' && ls_aufk-erdat+0(4).

        SELECT SINGLE txt
          FROM t003p
          INTO @DATA(lv_order_type)
          WHERE spras EQ 'E' AND
                auart EQ @ls_aufk-auart.
        IF sy-subrc EQ 0.
          ls_value-order_type = lv_order_type.
        ELSE.
          ls_value-order_type = ls_aufk-auart.

        ENDIF.

        ls_value-equipment_number = |{ ls_afih-equnr ALPHA = OUT } |.

        SELECT SINGLE eqktx
          FROM eqkt
          INTO @DATA(lv_eqktx)
          WHERE equnr EQ @ls_afih-equnr AND
                spras EQ 'E'.
        IF sy-subrc EQ 0.
          ls_value-equipment_description = lv_eqktx.

        ENDIF.

        ls_value-maintenance_plant = ls_afih-iwerk.
        SELECT SINGLE objid
          FROM crhd
          INTO @DATA(lv_objid)
          WHERE objty EQ 'A' AND
                arbpl EQ @ls_aufk-vaplz.
        IF sy-subrc EQ 0.
          SELECT SINGLE ktext
            FROM crtx
            INTO @DATA(lv_ktext)
            WHERE objty EQ 'A' AND
                  objid EQ @lv_objid AND
                  spras EQ 'E'.
          IF sy-subrc EQ 0.
            ls_value-work_center = lv_ktext.
          ELSE.
            ls_value-work_center = ls_aufk-vaplz.

          ENDIF.
        ELSE.
          ls_value-work_center = ls_aufk-vaplz.

        ENDIF.

        ls_value-notification_number = ls_afih-qmnum.

        ls_value-planned_start_date = ls_qmel-strmn+6(2) && '.' && ls_qmel-strmn+4(2) && '.' && ls_qmel-strmn+0(4).
        ls_value-planned_end_date = ls_qmel-ltrmn+6(2) && '.' && ls_qmel-ltrmn+4(2) && '.' && ls_qmel-ltrmn+0(4).
        IF event EQ 'RELEASED'.
          ls_value-current_status = 'Order Created, Released and In Process'.
        ELSEIF event EQ 'TECCOMPLETED'.
          ls_value-current_status = 'Order Technically Completed'.
        ELSEIF event EQ 'COMPLETED'.
          ls_value-current_status = 'Business Completion for Order'.

        ENDIF.

        ls_value-status_change_date = sy-datum+6(2) && '.' && sy-datum+4(2) && '.' && sy-datum+0(4).

        ls_publish_message-tag_name = 'sap-sb01-workorder'.
        ls_publish_message-timestamp = lv_unix_iat.
        ls_publish_message-value = ls_value.

        ls_publish_message_new-tag_name = 'sap-sb01-workorder'.
        ls_publish_message_new-timestamp = lv_unix_iat.
        ls_publish_message_new-type = 'sap'.
        ls_publish_message_new-value = ls_value.

        ls_publish_message_new-value-order_date = ls_aufk-erdat+0(4) && '-' && ls_aufk-erdat+4(2) && '-' && ls_aufk-erdat+6(2).
        ls_publish_message_new-value-planned_start_date = ls_qmel-strmn+0(4) && '-' && ls_qmel-strmn+4(2) && '-' && ls_qmel-strmn+6(2).
        ls_publish_message_new-value-planned_end_date = ls_qmel-ltrmn+0(4) && '-' && ls_qmel-ltrmn+4(2) && '-' && ls_qmel-ltrmn+6(2).
        ls_publish_message_new-value-status_change_date = sy-datum+0(4) && '-' && sy-datum+4(2) && '-' && sy-datum+6(2).

        TRY.
            CREATE OBJECT lo_client
              EXPORTING
                iv_key_name = 'DEMO_PUBSUB'.  "'DEMO_MDE'.

            lv_p_projects_id = lo_client->gv_project_id.

*            DATA(lv_json_obj) = /ui2/cl_json=>serialize( data = ls_publish_message
*                       pretty_name = /ui2/cl_json=>pretty_mode-camel_case
*                                                  ).
            DATA(lv_json_obj) = /ui2/cl_json=>serialize( data = ls_value
                       pretty_name = /ui2/cl_json=>pretty_mode-camel_case
                                                  ).

            ls_message-data = cl_http_utility=>encode_base64( unencoded = lv_json_obj ).
            APPEND ls_message TO ls_input-messages.

            CALL METHOD lo_client->publish_topics
              EXPORTING
                iv_p_projects_id = lv_p_projects_id
                iv_p_topics_id   = 'maintenance-order-status' "'input-messages'
                is_input         = ls_input
              IMPORTING
                es_output        = ls_output
                ev_ret_code      = lv_ret_code
                ev_err_text      = lv_err_text
                es_err_resp      = ls_err_resp.
          CATCH /goog/cx_sdk INTO lo_exception.
            lv_msg = lo_exception->get_text( ).
            RETURN.

        ENDTRY.

        TRY.
            CREATE OBJECT lo_client
              EXPORTING
                iv_key_name = 'DEMO_MDE_NEW'.

            lv_p_projects_id = lo_client->gv_project_id.

            lv_json_obj = /ui2/cl_json=>serialize( data = ls_publish_message_new
                       pretty_name = /ui2/cl_json=>pretty_mode-camel_case
                                                  ).

            ls_message-data = cl_http_utility=>encode_base64( unencoded = lv_json_obj ).
            APPEND ls_message TO ls_input-messages.

            CALL METHOD lo_client->publish_topics
              EXPORTING
                iv_p_projects_id = lv_p_projects_id
                iv_p_topics_id   = 'input-messages'
                is_input         = ls_input
              IMPORTING
                es_output        = ls_output
                ev_ret_code      = lv_ret_code
                ev_err_text      = lv_err_text
                es_err_resp      = ls_err_resp.
          CATCH /goog/cx_sdk INTO lo_exception.
            lv_msg = lo_exception->get_text( ).
            RETURN.

        ENDTRY.

        IF lo_client IS BOUND.
          lo_client->close( ).

        ENDIF.

      ENDIF.

    ENDIF.

  ENDIF.

ENDFUNCTION.
