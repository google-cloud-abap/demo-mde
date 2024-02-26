*&---------------------------------------------------------------------*
*& Report ZGOOG_CREATE_MAINT_ORDER
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zgoog_create_maint_order_new.

INCLUDE zgoog_maint_order_new_def.
INCLUDE zgoog_maint_order_new_sel.
INCLUDE zgoog_maint_order_new_impl.

START-OF-SELECTION.
  lcl_main=>execute( ).
