CLASS zcl_prueba_github DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_prueba_github IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.
  out->write( 'Respaldar en Github' ).
  ENDMETHOD.
ENDCLASS.
