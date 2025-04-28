CLASS zcl_work_order_validator_as DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: ty_char2 TYPE c LENGTH 2.
    METHODS:
      validate_create_order IMPORTING iv_customer_id   TYPE string
                                      iv_technician_id TYPE string
                                      iv_priority      TYPE c
                            RETURNING VALUE(rv_valid)  TYPE abap_bool,
      validate_update_order IMPORTING iv_work_order_id TYPE string
                                      iv_status        TYPE ty_char2
                            RETURNING VALUE(rv_valid)  TYPE abap_bool,
      validate_delete_order IMPORTING iv_work_order_id TYPE string
                                      iv_status        TYPE string
                            RETURNING VALUE(rv_valid)  TYPE abap_bool,
      validate_status_and_priority IMPORTING iv_status       TYPE ty_char2
                                             iv_priority     TYPE c
                                   RETURNING VALUE(rv_valid) TYPE abap_bool.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA lv_code TYPE string.

    METHODS check_customer_exists IMPORTING iv_customer_id   TYPE string
                                  RETURNING VALUE(rv_exists) TYPE abap_bool.

    METHODS check_technician_exists IMPORTING iv_technician_id TYPE string
                                    RETURNING VALUE(rv_exists) TYPE abap_bool.

    METHODS check_order_exists IMPORTING iv_work_order_id TYPE string
                               RETURNING VALUE(rv_exists) TYPE abap_bool.

    METHODS check_order_history IMPORTING iv_work_order_id TYPE string
                                RETURNING VALUE(rv_exists) TYPE abap_bool.

    METHODS check_priority_valid IMPORTING iv_priority      TYPE c
                                 RETURNING VALUE(rv_exists) TYPE abap_bool.

    METHODS check_status_valid IMPORTING iv_status        TYPE ty_char2
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
    DATA(lv_customer_exists) = check_customer_exists( iv_customer_id ).
    IF lv_customer_exists IS INITIAL.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    " Check if technician exists
    DATA(lv_technician_exists) = check_technician_exists( iv_technician_id ).
    IF lv_technician_exists IS INITIAL.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    " Check if priority exists and is valid
    DATA(lv_priority_valid) = check_priority_valid( iv_priority ).
    IF lv_priority_valid IS INITIAL.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    rv_valid = abap_true.
  ENDMETHOD.

  METHOD validate_update_order.

    " Check if the work order exists
    DATA(lv_order_exists) = check_order_exists( iv_work_order_id ).
    IF lv_order_exists IS INITIAL.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    " Check if the work order status exists and is valid
    DATA(lv_status_valid) = check_status_valid( iv_status ).
    IF lv_status_valid IS INITIAL.
      rv_valid = abap_false.
      RETURN.
    ENDIF.


    " Check if the order status is editable (e.g., Pending)

    IF iv_status NE 'PE'.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    rv_valid = abap_true.
  ENDMETHOD.

  METHOD validate_delete_order.
    " Check if the order exists
    DATA(lv_order_exists) = check_order_exists( iv_work_order_id ).
    IF lv_order_exists IS INITIAL.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    " Check if the order status is "PE" (Pending)
    IF iv_status NE 'PE'.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    " Check if the order has a history (i.e., if it has been modified before)
    DATA(lv_has_history) = check_order_history( iv_work_order_id ).
    IF lv_has_history IS NOT INITIAL.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    rv_valid = abap_true.
  ENDMETHOD.

  METHOD validate_status_and_priority.

    " Validate the status value
    DATA(lv_status_valid) = check_status_valid( iv_status ).
    IF lv_status_valid IS INITIAL.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    " Validate the priority value
    DATA(lv_priority_valid) = check_priority_valid( iv_priority ).
    IF lv_priority_valid IS INITIAL.
      rv_valid = abap_false.
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
    DATA(iv_condition) = |{ iv_field } = { iv_code }|.
    TRY.
        iv_condition =
          cl_abap_dyn_prg=>check_column_name(
            val = iv_condition
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
