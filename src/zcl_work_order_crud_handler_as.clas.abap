CLASS zcl_work_order_crud_handler_as DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
      create_work_order IMPORTING iv_work_order_id TYPE zde_work_order_id_as
                                  iv_customer_id   TYPE zde_customer_id_as
                                  iv_technician_id TYPE zde_technician_id_as
                                  iv_priority      TYPE zde_priority_as
                                  iv_description   TYPE zde_description_as
                        EXPORTING rv_result        TYPE abap_bool
                                  rv_message       TYPE string,

      read_work_order IMPORTING iv_work_order_id TYPE zde_work_order_id_as

                      EXPORTING rv_result        TYPE abap_bool
                                rv_message       TYPE string
                                rv_work_order    TYPE zdt_work_order_a,

      update_work_order IMPORTING iv_work_order_id TYPE zde_work_order_id_as
                                  iv_customer_id   TYPE zde_customer_id_as
                                  iv_technician_id TYPE zde_technician_id_as
                                  iv_priority      TYPE zde_priority_as
                                  iv_status        TYPE zde_status_as
                                  iv_description   TYPE zde_description_as

                        EXPORTING rv_result        TYPE abap_bool
                                  rv_message       TYPE string,

      delete_work_order IMPORTING iv_work_order_id TYPE zde_work_order_id_as
                        EXPORTING rv_result        TYPE abap_bool
                                  rv_message       TYPE string.


  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_work_order_crud_handler_as IMPLEMENTATION.

  METHOD create_work_order.

    DATA: ls_work_order TYPE zdt_work_order_a.

    DATA(lo_work_order) = NEW zcl_work_order_validator_as( ).


    IF lo_work_order->validate_create_order( EXPORTING iv_customer_id   = iv_customer_id
                                                      iv_technician_id = iv_technician_id
                                                       iv_priority      = iv_priority
                                             IMPORTING rv_message    = rv_message          ) = abap_false.

      rv_result = abap_false.
      RETURN.
    ENDIF.

    ls_work_order = VALUE #( work_order_id = iv_work_order_id
                            customer_id   = iv_customer_id
                            technician_id = iv_technician_id
                            priority      = iv_priority
                            status        = 'PE'
                            description   = iv_description
                            creation_date = cl_abap_context_info=>get_system_date( ) ).
    TRY.
        INSERT zdt_work_order_a FROM @ls_work_order.
      CATCH cx_sy_open_sql_db INTO DATA(lx_sql_db).
        rv_message = | { TEXT-011 } { iv_work_order_id }{ lx_sql_db->get_text(  ) }|.
        rv_result = abap_false.
        RETURN.
    ENDTRY.

    IF sy-subrc = 0.
      rv_message = | { TEXT-012 } { iv_work_order_id }|.
      rv_result = abap_true.
    ELSE.
      rv_message = | { TEXT-013 } { iv_work_order_id }|.
      rv_result = abap_false.
    ENDIF.

  ENDMETHOD.

  METHOD read_work_order.
    SELECT SINGLE FROM zdt_work_order_a
    FIELDS *
    WHERE work_order_id = @iv_work_order_id
    INTO @rv_work_order.

    IF sy-subrc = 0.
      CLEAR rv_message.
      rv_result = abap_true.

    ELSE.
      rv_message = | { TEXT-014 } { iv_work_order_id }|.
      rv_result = abap_false.
    ENDIF.


  ENDMETHOD.

  METHOD delete_work_order.

    DATA(lo_work_order) = NEW zcl_work_order_validator_as( ).

    IF lo_work_order->validate_delete_order( EXPORTING iv_work_order_id   = iv_work_order_id
                                             IMPORTING rv_message = rv_message ) = abap_false.
      rv_result = abap_false.
      RETURN.
    ENDIF.
    TRY.
        DELETE FROM zdt_work_order_a
        WHERE work_order_id = @iv_work_order_id.
        IF sy-subrc = 0.
          rv_result = abap_true.
          RETURN.
        ELSE.
          rv_result = abap_false.
          rv_message = | { TEXT-016 } { iv_work_order_id }|.
          RETURN.
        ENDIF.
      CATCH cx_sy_open_sql_db INTO DATA(lx_sql_db).
        rv_message = | { TEXT-015 } { iv_work_order_id }{ lx_sql_db->get_text(  ) }|.
        rv_result = abap_false.
        RETURN.

    ENDTRY.




  ENDMETHOD.


  METHOD update_work_order.

    DATA: lv_changes TYPE string,
          lt_set TYPE TABLE OF string.
    DATA(lo_work_order) = NEW zcl_work_order_validator_as( ).

    IF lo_work_order->validate_update_order( EXPORTING iv_work_order_id   = iv_work_order_id
                                             IMPORTING rv_message = rv_message ) = abap_false.

      rv_result = abap_false.
      RETURN.
    ENDIF.


* validate to if the customer, technician or priority modify exists.



    IF lo_work_order->validate_create_order( EXPORTING iv_customer_id   = iv_customer_id
                                                        iv_technician_id = iv_technician_id
                                                          iv_priority      = iv_priority
                                            IMPORTING rv_message = rv_message ) = abap_false.
      rv_result = abap_false.
      RETURN.
    ENDIF.


    SELECT SINGLE FROM zdt_work_order_a
           FIELDS *
           WHERE work_order_id = @iv_work_order_id
           INTO @DATA(ls_work_order).

    "Por temas de auditoria el campo "creation date" no se puede cambiar, toma la fecha del sistema
    clear lt_set.
    IF sy-subrc = 0.

      IF ls_work_order-customer_id   NE iv_customer_id.
        lv_changes = | { ` ` } Customer Before : { ls_work_order-customer_id }    After : { iv_customer_id } |.
        APPEND |customer_id = '{ iv_customer_id }'| TO lt_set.
      ENDIF.

      IF ls_work_order-technician_id   NE iv_technician_id.
        lv_changes = lv_changes && | { ` ` } Technician Before : { ls_work_order-technician_id }    After : { iv_technician_id  } |.
        APPEND |technician_id = '{ iv_technician_id }'| TO lt_set.
      ENDIF.

      IF ls_work_order-priority   NE iv_priority.
        lv_changes = lv_changes && | { ` ` } Priority Before: { ls_work_order-Priority }    After : { iv_Priority  } |.
        APPEND |priority = '{ iv_priority }'| TO lt_set.
      ENDIF.

      IF ls_work_order-status   NE iv_status.
        lv_changes = lv_changes && | { ` ` } Status Before: { ls_work_order-status }    After : { iv_status  } |.
        APPEND |status = '{ iv_status }'| TO lt_set.
      ENDIF.

      IF ls_work_order-description   NE iv_description.
        lv_changes = lv_changes && | { ` ` } Description Before: { ls_work_order-description }    After : { iv_description  } |.
        APPEND |description = '{ iv_description }'| TO lt_set.
      ENDIF.



      TRY.
          UPDATE zdt_work_order_a
          SET (lt_set)
          WHERE work_order_id = @iv_work_order_id.

        CATCH cx_sy_open_sql_db INTO DATA(lx_sql_db).
          rv_message =  | { TEXT-017 } { iv_work_order_id }{ lx_sql_db->get_text(  ) }|.
          rv_result = abap_false.
          RETURN.
      ENDTRY.

      IF sy-subrc = 0.

        "  now creating the history
        "  select all the rows in table

        DATA: ls_wohist TYPE zwork_ordhist_05.
        DATA: lv_count TYPE i.

        DATA: lv_date TYPE d.
        lv_date = cl_abap_context_info=>get_system_date( ).

        SELECT COUNT(*)
          FROM zwork_ordhist_05
          INTO @lv_count.

        ls_wohist = VALUE #( history_id = lv_count + 1
                             work_order_id = iv_work_order_id
                          change_description   = lv_changes
                          modification_date  = lv_date
                           ).

        INSERT zwork_ordhist_05 FROM @ls_wohist.
        IF sy-subrc = 0.
          rv_message =  | Work order { iv_work_order_id } updated correctly with history in table|  .
          rv_result = abap_true.
        ELSE.
          rv_message =  | Work order { iv_work_order_id } not updated correctly with history in table|  .
          rv_result = abap_true.
        ENDIF.

      ELSE.
        rv_message =  | Work order { iv_work_order_id } was not updated correctly in table|  .
        rv_result = abap_false.
      ENDIF.


    ENDIF.

  ENDMETHOD.


ENDCLASS.
