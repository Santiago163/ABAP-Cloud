CLASS zcl_initial_data_as DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_initial_data_as IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.

  MODIFY zdt_status_as FROM TABLE @( VALUE #( ( status_code = 'PE' status_description = 'Pending' )
                                    ( status_code = 'CO' status_description = 'Completed'  ) ) ).

  MODIFY zdt_priority_as FROM TABLE @( VALUE #( ( priority = 'A' priority_description = 'High' )
                                    ( priority = 'B' priority_description = 'Low'  ) ) ).




  ENDMETHOD.
ENDCLASS.
