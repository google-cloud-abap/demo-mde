*&---------------------------------------------------------------------*
*& Include          ZGOOG_CREATE_MAINT_ORDER_SEL
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS:
    p_key    TYPE /goog/keyname OBLIGATORY MATCHCODE OBJECT /goog/sh_gcp_key_nm,
    p_subsid TYPE string OBLIGATORY LOWER CASE.

SELECTION-SCREEN END OF BLOCK b1.
