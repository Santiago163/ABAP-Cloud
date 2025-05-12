CLASS zcl_work_order_crud_test_as DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS: test_create_work_order IMPORTING output  TYPE REF TO if_oo_adt_classrun_out,
      test_read_work_order IMPORTING output  TYPE REF TO if_oo_adt_classrun_out,
      test_update_work_order IMPORTING output  TYPE REF TO if_oo_adt_classrun_out,
      test_delete_work_order IMPORTING output  TYPE REF TO if_oo_adt_classrun_out.

    INTERFACES if_oo_adt_classrun .
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: rv_result  TYPE abap_bool,
          rv_message TYPE string,
          lv_user    TYPE zde_user_as.


    METHODS initial_data IMPORTING output  TYPE REF TO if_oo_adt_classrun_out.

ENDCLASS.

CLASS zcl_work_order_crud_test_as IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.

    initial_data( output = out ).
    test_create_work_order( output = out ).
    test_delete_work_order( output = out ).
    test_read_work_order( output = out ).
    test_update_work_order( output = out ).

  ENDMETHOD.

  METHOD initial_data.


    MODIFY zdt_status_as FROM TABLE @( VALUE #( ( status_code = 'PE' status_description = 'Pending' )
                                    ( status_code = 'CO' status_description = 'Completed'  ) ) ).

    MODIFY zdt_priority_as FROM TABLE @( VALUE #( ( priority = 'A' priority_description = 'High' )
                                      ( priority = 'B' priority_description = 'Low'  ) ) ).

    MODIFY zdt_customer_as FROM TABLE @( VALUE #( ( customer_id = '1001' name = 'Mateo López' address = 'Quito Ecuador' phone = '998765432' )
                                                  ( customer_id = '1002' name = 'Sofía Fernández' address = 'Guayaquil Ecuador' phone = '987654321' )
                                                   ( customer_id = '1003' name = 'Leonardo Pérez' address = 'Cuenca Ecuador' phone = '976543210' )
                                                   ( customer_id = '1004' name = 'Valeria Rojas' address = 'Loja Ecuador' phone = '965432109' )
                                                   ( customer_id = '1005' name = 'Sebastián Gómez' address = 'Ambato Ecuador' phone = '954321098' )
                                                    ) ).
    MODIFY zdt_technician_a FROM TABLE @( VALUE #( ( technician_id = 'T001' name = 'Carlos Medina' specialty = 'Telecomunicaciones' )
                                                   ( technician_id = 'T002' name = 'Andrea Torres' specialty = 'Seguridad ' )
                                                   ( technician_id = 'T003' name = 'Luis Ramírez' specialty = 'Soporte Técnico' )
                                                   ( technician_id = 'T004' name = 'Sofía Pérez' specialty = 'Desarrollo Web' )
                                                   ( technician_id = 'T005' name = 'Ricardo Gómez' specialty = 'Bases de Datos' )
                                                    ) ).
    Output->write( 'Test data has been aggregated' ).
    DELETE FROM zdt_work_ord_h_a
    WHERE 1 = 1.
    DELETE FROM zdt_WORK_ORDER_A
    WHERE 1 = 1.
  ENDMETHOD.

  METHOD test_create_work_order.

    DATA lv_work_order TYPE zde_work_order_id VALUE '0001' .
    DATA(lo_work_order) = NEW zcl_work_order_crud_handler_as(  ).
    lv_user = sy-uname.

    AUTHORITY-CHECK OBJECT 'ZAO_USER_A'
      ID 'ZAF_USER_A' FIELD lv_user
      ID 'ACTVT' FIELD '01'. " 01 = CREAR

    IF sy-subrc NE 0.
      rv_result = abap_false.
      rv_message = | { TEXT-001 } { lv_user } |.
      output->write( rv_message ).
*      RETURN.                 Toggle comment on due to the authorization object (ZAO_USER_A) is not assigned
    ENDIF.


    lo_work_order->create_work_order( EXPORTING iv_work_order_id = lv_work_order
                                                iv_customer_id = '00001001'
                                                iv_technician_id = 'T001'
                                                iv_priority = 'B'
                                                iv_description = 'Maintennance request'
                                      IMPORTING rv_result      = rv_result
                                                rv_message     = rv_message ).
    IF rv_result = abap_false.
      output->write( rv_message ).
    ENDIF.
    lv_work_order = '0002'.
    lo_work_order->create_work_order( EXPORTING iv_work_order_id = lv_work_order
                                                iv_customer_id = '00001001'
                                                iv_technician_id = 'T002'
                                                iv_priority = 'A'
                                                iv_description = 'Seguridad'
                                      IMPORTING rv_result      = rv_result
                                                rv_message     = rv_message ).
    IF rv_result = abap_false.
      output->write( rv_message ).
    ENDIF.
    lv_work_order = '0003'.
    lo_work_order->create_work_order( EXPORTING iv_work_order_id = lv_work_order
                                                iv_customer_id = '00001001'
                                                iv_technician_id = 'T003'
                                                iv_priority = 'A'
                                                iv_description = 'Soporte Tecnico'
                                      IMPORTING rv_result      = rv_result
                                                rv_message     = rv_message ).
    IF rv_result = abap_false.
      output->write( rv_message ).
    ENDIF.

    SELECT  FROM zdt_work_order_a
    FIELDS *
    INTO TABLE @DATA(lt_work_order).
    output->write( name = 'WORK ORDER GENERATED ' data = lt_work_order ).


  ENDMETHOD.

  METHOD test_delete_work_order.
    DATA lv_work_order TYPE zde_work_order_id VALUE '0001' .
    DATA(lo_work_order) = NEW zcl_work_order_crud_handler_as(  ).
    lv_user = sy-uname.

    AUTHORITY-CHECK OBJECT 'ZAO_USER_A'
      ID 'ZAF_USER_A' FIELD lv_user
      ID 'ACTVT' FIELD '06'. " 06 = BORRAR

    IF sy-subrc NE 0.
      rv_result = abap_false.
      rv_message = | { TEXT-002 } { lv_user } |.
      output->write( rv_message ).
*      RETURN.                 Toggle comment on due to the authorization object (ZAO_USER_A) is not assigned
    ENDIF.

    lo_work_order->delete_work_order( EXPORTING iv_work_order_id = lv_work_order
                                      IMPORTING rv_result = rv_result
                                                rv_message = rv_message ).

    IF rv_result = abap_false.
      output->write( rv_message ).
    ENDIF.

    SELECT  FROM zdt_work_order_a
    FIELDS *
    INTO TABLE @DATA(lt_work_order).
    output->write( name = 'DELETE WORK ORDER' data = lt_work_order ).

  ENDMETHOD.

  METHOD test_read_work_order.
    DATA lv_work_order TYPE zde_work_order_id VALUE '0002' .
    DATA(lo_work_order) = NEW zcl_work_order_crud_handler_as(  ).
    lv_user = sy-uname.

    AUTHORITY-CHECK OBJECT 'ZAO_USER_A'
      ID 'ZAF_USER_A' FIELD lv_user
      ID 'ACTVT' FIELD '03'. " 01 = LEER

    IF sy-subrc NE 0.
      rv_result = abap_false.
      rv_message = | { TEXT-003 } { lv_user } |.
      output->write( rv_message ).
*      RETURN.                 Toggle comment on due to the authorization object (ZAO_USER_A) is not assigned
    ENDIF.

    lo_work_order->read_work_order( EXPORTING iv_work_order_id = lv_work_order
                                    IMPORTING rv_result = rv_result
                                              rv_message = rv_message
                                              rv_work_order = DATA(ls_work_order) ).
    IF rv_result = abap_false.
      output->write( rv_message ).
    ELSE.
      output->write( name = 'READ WORK ORDER' data = ls_work_order ).
    ENDIF.

  ENDMETHOD.

  METHOD test_update_work_order.
    DATA lv_work_order TYPE zde_work_order_id VALUE '0003' .
    DATA(lo_work_order) = NEW zcl_work_order_crud_handler_as(  ).
    lv_user = sy-uname.

    AUTHORITY-CHECK OBJECT 'ZAO_USER_A'
      ID 'ZAF_USER_A' FIELD lv_user
      ID 'ACTVT' FIELD '02'. " 02 = UPDATE

    IF sy-subrc NE 0.
      rv_result = abap_false.
      rv_message = | { TEXT-004 } { lv_user } |.
      output->write( rv_message ).
*      RETURN.                 Toggle comment on due to the authorization object (ZAO_USER_A) is not assigned
    ENDIF.

    lo_work_order->update_work_order( EXPORTING  iv_work_order_id = lv_work_order
                                                  iv_customer_id  = '1004'
                                                  iv_technician_id = 'T004'
                                                  iv_priority    = 'B'
                                                  iv_status      = 'CO'
                                                  iv_description   = 'Soporte Tecnico'
                                      IMPORTING   rv_result = rv_result
                                                  rv_message = rv_message            ).
    IF rv_result = abap_false.
      output->write( rv_message ).
    ELSE.

      SELECT SINGLE FROM zdt_work_order_a
      FIELDS *
      WHERE work_order_id = @lv_work_order
      INTO @DATA(ls_work_order).
      output->write( name = 'UPDATE WORK ORDER' data = ls_work_order ).

      SELECT FROM zdt_work_ord_h_a
      FIELDS *
      WHERE work_order_id = @lv_work_order
      INTO TABLE @DATA(lt_work_order).
      output->write( name = 'CHANGE LOG' data = lt_work_order ).

    ENDIF.

  ENDMETHOD.

ENDCLASS.
