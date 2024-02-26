*&---------------------------------------------------------------------*
*& Include          ZGOOG_CREATE_MAINT_ORDER_DEF
*&---------------------------------------------------------------------*

CLASS lcl_main DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:
      execute.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF mty_output,
        aufart  TYPE aufart,
        equnr   TYPE equnr,
        gewrk   TYPE gewrk,
        remarks TYPE string,
        aufnr   TYPE aufnr,

      END OF mty_output.

    TYPES:
      mtt_string TYPE STANDARD TABLE OF string,
      mtt_output TYPE STANDARD TABLE OF mty_output.

    CONSTANTS:
      mc_s TYPE char1 VALUE 'S',
      mc_e TYPE char1 VALUE 'E'.

    CLASS-METHODS:
      pull_pubsub_messages
        EXPORTING
          ex_t_messages TYPE mtt_string,
      create_maintenance_order
        IMPORTING
          im_t_messages TYPE mtt_string
        EXPORTING
          ex_t_output   TYPE mtt_output,
      display_output
        IMPORTING
          im_t_output   TYPE mtt_output.

ENDCLASS.
