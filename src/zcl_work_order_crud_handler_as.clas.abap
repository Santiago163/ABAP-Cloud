CLASS zcl_work_order_crud_handler_as DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
    " No se solicita la fecha de cración porque se utiliza la fecha del sistema por defecto.
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

    " No se solicita la fecha de cración porque se utiliza la fecha del sistema por defecto.
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
    "Realiza la validación y crea una nueva orden de trabajo si es válida.
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
    "Lee los detalles de una orden de trabajo existente.

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
    "Elimina una orden de trabajo si cumple con los requisitos de validación.
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
  "Realiza una actualización de una orden de trabajo, validando antes de la modificación.

    DATA lv_changes TYPE string.
    DATA lt_set     TYPE TABLE OF string.
    DATA lt_parameter TYPE if_abap_lock_object=>tt_parameter.
    DATA lv_set TYPE string.
    DATA lv_ix TYPE i.

    DATA(lo_work_order) = NEW zcl_work_order_validator_as( ).

    IF lo_work_order->validate_update_order( EXPORTING iv_work_order_id = iv_work_order_id
                                             IMPORTING rv_message       = rv_message ) = abap_false.

      rv_result = abap_false.
      RETURN.
    ENDIF.

    " validate to if the customer, technician or priority modify exists.

    IF lo_work_order->validate_create_order( EXPORTING iv_customer_id   = iv_customer_id
                                                       iv_technician_id = iv_technician_id
                                                       iv_priority      = iv_priority
                                             IMPORTING rv_message       = rv_message ) = abap_false.
      rv_result = abap_false.
      RETURN.
    ENDIF.

    SELECT SINGLE FROM zdt_work_order_a
      FIELDS  customer_id, technician_id, priority, status, description
      WHERE work_order_id = @iv_work_order_id
      INTO @DATA(ls_work_order).

    " Por temas de auditoria el campo "creation date" no se puede cambiar, toma la fecha del sistema
    CLEAR lt_set.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

"La clase CL_ABAP_DYN_PRG en ABAP Cloud se utiliza para validar nombres de tablas en consultas dinámicas y evitar riesgos de SQL Injection

    IF ls_work_order-customer_id <> iv_customer_id.
      lv_changes = | { ` ` } Customer Before : { ls_work_order-customer_id }    After : { iv_customer_id } |.
      lv_set = iv_customer_id.
      APPEND |customer_id = | && cl_abap_dyn_prg=>quote( lv_set ) TO lt_set.

    ENDIF.

    IF ls_work_order-technician_id <> iv_technician_id.
      lv_changes = |{ lv_changes } { ` ` } Technician Before : { ls_work_order-technician_id }    After : { iv_technician_id } |.
      lv_set = iv_technician_id.
      lv_ix = lines( lt_set ).
      IF lv_ix >= 1.
        lt_set[ lv_ix ] &&= ','.
      ENDIF.
      APPEND  | technician_id = | && cl_abap_dyn_prg=>quote( lv_set ) TO lt_set.
    ENDIF.

    IF ls_work_order-priority <> iv_priority.
      lv_changes = |{ lv_changes } { ` ` } Priority Before: { ls_work_order-Priority }    After : { iv_Priority } |.
      lv_set = iv_priority.
      lv_ix = lines( lt_set ).
      IF lv_ix >= 1.
        lt_set[ lv_ix ] &&= ','.
      ENDIF.
      APPEND |priority = | && cl_abap_dyn_prg=>quote( lv_set ) TO lt_set.
    ENDIF.

    IF ls_work_order-status <> iv_status.
      lv_changes = |{ lv_changes } { ` ` } Status Before: { ls_work_order-status }    After : { iv_status } |.
      lv_set = iv_status.
      lv_ix = lines( lt_set ).
      IF lv_ix >= 1.
        lt_set[ lv_ix ] &&= ','.
      ENDIF.
      APPEND |status = | && cl_abap_dyn_prg=>quote( lv_set ) TO lt_set.
    ENDIF.

    IF ls_work_order-description <> iv_description.
      lv_changes = |{ lv_changes } { ` ` } Description Before: { ls_work_order-description }    After : { iv_description } |.
      lv_set = iv_description.
      lv_ix = lines( lt_set ).
      IF lv_ix >= 1.
        lt_set[ lv_ix ] &&= ','.
      ENDIF.
      APPEND |description = | && cl_abap_dyn_prg=>quote( lv_set ) TO lt_set.
    ENDIF.

    TRY.
        DATA(lo_block_order) = cl_abap_lock_object_factory=>get_instance( iv_name = 'EZ_WORK_ORDER_AS' ).
      CATCH cx_abap_lock_failure INTO DATA(lx_sql_lock).
        rv_message = | { TEXT-021 } { iv_work_order_id }{ lx_sql_lock->get_text( ) }|.
        rv_result = abap_false.
        RETURN.

    ENDTRY.

    TRY.
        lo_block_order->enqueue(                                 "Bloqueo
*      it_table_mode =
      it_parameter  = lt_parameter
*      _scope        =
*      _wait         =
        ).
        " handle exception

      CATCH cx_abap_foreign_lock INTO DATA(lx_foreign_lock).
        rv_message = | { TEXT-022 } { iv_work_order_id }{ lx_foreign_lock->get_text( ) }|.
        rv_result = abap_false.
        RETURN.
      CATCH cx_abap_lock_failure INTO DATA(lx_lock_failure).
        rv_message = | { TEXT-023 } { iv_work_order_id }{ lx_lock_failure->get_text( ) }|.
        rv_result = abap_false.
        RETURN.
    ENDTRY.




    TRY.
        UPDATE zdt_work_order_a
        SET (lt_set)
        WHERE work_order_id = @iv_work_order_id.

      CATCH cx_sy_open_sql_db INTO DATA(lx_sql_db).
        rv_message = | { TEXT-017 } { iv_work_order_id }{ lx_sql_db->get_text( ) }|.
        rv_result = abap_false.
        RETURN.
    ENDTRY.

    IF sy-subrc = 0.

      DATA ls_work_order_his TYPE zdt_work_ord_h_a.

      SELECT COUNT(*) + 1 FROM zdt_work_ord_h_a
      WHERE 1 = 1
        INTO @DATA(lv_count).

      ls_work_order_his = VALUE #( history_id         = CONV zde_history_id_as( lv_count )
                                   work_order_id      = iv_work_order_id
                                   change_description = lv_changes
                                   modification_date  = cl_abap_context_info=>get_system_date( ) ).

      TRY.
          INSERT zdt_work_ord_h_a FROM @ls_work_order_his.
        CATCH cx_sy_open_sql_db INTO DATA(lx_sql_db_i).
          rv_message = | { TEXT-020 } { iv_work_order_id }{ lx_sql_db_i->get_text( ) }|.
          rv_result = abap_false.
          ROLLBACK WORK.                                  "Deshace todos los cambios
          TRY.
              lo_block_order->dequeue(
             it_parameter  = lt_parameter   ).
            CATCH cx_abap_lock_failure INTO DATA(lx_dequeue).
              rv_message = | { TEXT-024 } { iv_work_order_id }{ lx_dequeue->get_text( ) }|.
              rv_result = abap_false.
          ENDTRY.

          RETURN.
      ENDTRY.

      IF sy-subrc = 0.
        CLEAR rv_message.
        rv_result = abap_true.
        COMMIT WORK.                                       "Compromete los cambios
      ELSE.
        rv_message = | { TEXT-018 } { iv_work_order_id }|.
        rv_result = abap_false.
        ROLLBACK WORK.
      ENDIF.
      TRY.
          lo_block_order->dequeue(                                                  "Desbloqueo
         it_parameter  = lt_parameter   ).
        CATCH cx_abap_lock_failure INTO lx_dequeue.
          rv_message = | { TEXT-024 } { iv_work_order_id }{ lx_dequeue->get_text( ) }|.
          rv_result = abap_false.

      ENDTRY.

    ELSE.
      rv_message = | { TEXT-019 } { iv_work_order_id }|.
      rv_result = abap_false.
    ENDIF.
  ENDMETHOD.


ENDCLASS.
