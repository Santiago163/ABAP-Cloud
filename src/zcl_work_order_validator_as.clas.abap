CLASS zcl_work_order_validator_as DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
      validate_create_order IMPORTING iv_customer_id   TYPE zde_customer_id_as
                                      iv_technician_id TYPE zde_technician_id_as
                                      iv_priority      TYPE zde_priority_as
                            EXPORTING rv_message       TYPE string
                            RETURNING VALUE(rv_valid)  TYPE abap_bool,
      validate_update_order IMPORTING iv_work_order_id TYPE zde_work_order_id_as
                            EXPORTING rv_message       TYPE string
                            RETURNING VALUE(rv_valid)  TYPE abap_bool,
      validate_delete_order IMPORTING iv_work_order_id TYPE zde_work_order_id_as
                            EXPORTING rv_message       TYPE string
                            RETURNING VALUE(rv_valid)  TYPE abap_bool,
      validate_status_and_priority IMPORTING iv_status       TYPE zde_status_as
                                             iv_priority     TYPE zde_priority_as
                                   EXPORTING rv_message      TYPE string
                                   RETURNING VALUE(rv_valid) TYPE abap_bool.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA lv_code TYPE string.

    METHODS check_customer_exists IMPORTING iv_customer_id   TYPE zde_customer_id_as
                                  RETURNING VALUE(rv_exists) TYPE abap_bool.

    METHODS check_technician_exists IMPORTING iv_technician_id TYPE zde_technician_id_as
                                    RETURNING VALUE(rv_exists) TYPE abap_bool.

    METHODS check_order_exists IMPORTING iv_work_order_id TYPE zde_work_order_id_as
                               RETURNING VALUE(rv_exists) TYPE abap_bool.

    METHODS check_order_history IMPORTING iv_work_order_id TYPE zde_work_order_id_as
                                RETURNING VALUE(rv_exists) TYPE abap_bool.

    METHODS check_priority_valid IMPORTING iv_priority      TYPE zde_priority_as
                                 RETURNING VALUE(rv_exists) TYPE abap_bool.

    METHODS check_status_valid IMPORTING iv_status        TYPE zde_status_as
                               RETURNING VALUE(rv_exists) TYPE abap_bool.

    METHODS execute_query IMPORTING iv_table         TYPE string
                                    iv_field         TYPE string
                                    iv_code          TYPE string
                          RETURNING VALUE(rv_exists) TYPE abap_bool
                          RAISING   cx_sy_sql_error.
ENDCLASS.



CLASS zcl_work_order_validator_as IMPLEMENTATION.
  METHOD validate_create_order.
    " Check if customer exists
    rv_valid = check_customer_exists( iv_customer_id ).
    IF rv_valid = abap_false.
      rv_message = | { TEXT-001 } { iv_customer_id }|.
      RETURN.
    ENDIF.

    " Check if technician exists
    rv_valid = check_technician_exists( iv_technician_id ).
    IF rv_valid = abap_false.
      rv_message = | { TEXT-002 } { iv_technician_id }|.
      RETURN.
    ENDIF.

    " Check if priority exists and is valid
    rv_valid = check_priority_valid( iv_priority ).
    IF rv_valid = abap_false.
      rv_message = | { TEXT-003 } { iv_priority }|.
      RETURN.
    ENDIF.
    CLEAR rv_message.
    rv_valid = abap_true.
  ENDMETHOD.

  METHOD validate_update_order.

    " Check if the work order exists
    rv_valid = check_order_exists( iv_work_order_id ).
    IF rv_valid = abap_false.
      rv_message = | { TEXT-004 } { iv_work_order_id }|.
      RETURN.
    ENDIF.

    " Check if the work order status exists and is valid

    DATA(lo_work_order) = NEW zcl_work_order_crud_handler_as( ).
    lo_work_order->read_work_order(  EXPORTING iv_work_order_id = iv_work_order_id
                                     IMPORTING rv_result   = rv_valid
                                               rv_message  = rv_message
                                               rv_work_order  = DATA(ls_work_order)    ).
    " Check if the order status is "PE" (Pending)
    IF ls_work_order-status NE 'PE'.
      rv_valid = abap_false.
      rv_message = | { TEXT-007 } { ls_work_order-status }|.
      RETURN.
    ENDIF.


    rv_valid = abap_true.
  ENDMETHOD.

  METHOD validate_delete_order.

    " Check if the order exists
    rv_valid = check_order_exists( iv_work_order_id ).
    IF rv_valid = abap_false.
      rv_message = | { TEXT-004 } { iv_work_order_id }|.
      RETURN.
    ENDIF.

    DATA(lo_work_order) = NEW zcl_work_order_crud_handler_as( ).
    lo_work_order->read_work_order(  EXPORTING iv_work_order_id = iv_work_order_id
                                     IMPORTING rv_result   = rv_valid
                                               rv_message  = rv_message
                                               rv_work_order  = DATA(ls_work_order)    ).
    " Check if the order status is "PE" (Pending)
    IF ls_work_order-status NE 'PE'.
      rv_valid = abap_false.
      rv_message = | { TEXT-007 } { ls_work_order-status }|.
      RETURN.
    ENDIF.

    " Check if the order has a history (i.e., if it has been modified before)
    rv_valid = check_order_history( iv_work_order_id ).
    IF rv_valid = abap_true.
      rv_message = | { TEXT-008 } { iv_work_order_id }|.
      RETURN.
    ENDIF.

    rv_valid = abap_true.
  ENDMETHOD.

  METHOD validate_status_and_priority.

    " Validate the status value
    rv_valid = check_status_valid( iv_status ).
    IF rv_valid = abap_false.
      rv_message = | { TEXT-009 } { iv_status }|.
      RETURN.
    ENDIF.

    " Validate the priority value
    rv_valid = check_priority_valid( iv_priority ).
    IF rv_valid = abap_false.
      rv_message = | { TEXT-010 } { iv_priority }|.
      RETURN.
    ENDIF.
    rv_valid = abap_true.
  ENDMETHOD.


  METHOD check_customer_exists.

    lv_code = iv_customer_id.
    rv_exists = execute_query( iv_table = 'zdt_customer_as'
                               iv_field = 'customer_id'
                               iv_code =  lv_code ).
  ENDMETHOD.


  METHOD check_technician_exists.

    lv_code = iv_technician_id.
    rv_exists = execute_query( iv_table = 'zdt_technician_a'
                               iv_field = 'technician_id'
                               iv_code =  lv_code ).
  ENDMETHOD.


  METHOD check_order_exists.
    lv_code = iv_work_order_id.
    rv_exists = execute_query( iv_table = 'zdt_work_order_a'
                               iv_field = 'work_order_id'
                               iv_code =  lv_code ).
  ENDMETHOD.

  METHOD check_order_history.
    lv_code = iv_work_order_id.
    rv_exists = execute_query( iv_table = 'zdt_work_ord_h_a'
                               iv_field = 'history_id'
                               iv_code =  lv_code ).
  ENDMETHOD.

  METHOD check_priority_valid.
    lv_code = iv_priority.
    rv_exists = execute_query( iv_table = 'zdt_priority_as'
                               iv_field = 'priority'
                               iv_code =  lv_code ).
  ENDMETHOD.

  METHOD check_status_valid.
    lv_code = iv_status.
    rv_exists = execute_query( iv_table = 'zdt_status_as'
                               iv_field = 'status_code'
                               iv_code =  lv_code ).
  ENDMETHOD.

  METHOD execute_query.

    CONSTANTS entry_exists TYPE abap_bool VALUE abap_true.
    DATA(iv_condition) = |{ iv_field } = '{ iv_code }'|.
    TRY.
        cl_abap_dyn_prg=>check_column_name(
          val = iv_field
          strict = abap_true ).
      CATCH cx_abap_invalid_name INTO DATA(lx_abap_invalid_name).
        RAISE SHORTDUMP TYPE cx_abap_invalid_name EXPORTING previous = lx_abap_invalid_name..
        "handle exception
    ENDTRY.

    TRY.
        SELECT SINGLE
        FROM (iv_table)
        FIELDS: @entry_exists
        WHERE  (iv_condition)
        INTO @DATA(lv_exists).
      CATCH cx_sy_dynamic_osql_syntax  INTO DATA(lx_dynamic_osq_syntax).
        RAISE SHORTDUMP TYPE cx_sy_dynamic_osql_syntax EXPORTING previous = lx_dynamic_osq_syntax.
      CATCH cx_sy_dynamic_osql_semantics  INTO DATA(lx_dynamic_osql_semantics).
        RAISE SHORTDUMP TYPE cx_sy_dynamic_osql_syntax EXPORTING previous = lx_dynamic_osql_semantics.
      CATCH cx_sy_dynamic_osql_error   INTO DATA(lx_dynamic_osql_error).
        RAISE SHORTDUMP TYPE cx_sy_dynamic_osql_syntax EXPORTING previous = lx_dynamic_osql_error.
    ENDTRY.


    IF sy-subrc = 0.
      rv_exists = abap_true.
    ELSE.
      rv_exists = abap_false.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
