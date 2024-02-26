*&---------------------------------------------------------------------*
*& Report ZGOOG_CREATE_MAINT_ORDER
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zgoog_create_maint_order.

INCLUDE zgoog_create_maint_order_def.
INCLUDE zgoog_create_maint_order_sel.
INCLUDE zgoog_create_maint_order_impl.

START-OF-SELECTION.
  lcl_main=>execute( ).
