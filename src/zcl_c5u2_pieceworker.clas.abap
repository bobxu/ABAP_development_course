CLASS zcl_c5u2_pieceworker DEFINITION
  PUBLIC
  INHERITING FROM zcl_c5u2_employee
  FINAL
  CREATE PUBLIC .

PUBLIC SECTION.
   METHODS:
      constructor IMPORTING fn TYPE string
                            ln TYPE string
                            wg TYPE f
                            qu TYPE i,
      setwage IMPORTING wg TYPE f,
      setquan IMPORTING qu TYPE i,
      earnings REDEFINITION,
      print REDEFINITION.
PROTECTED SECTION.
PRIVATE SECTION.
    DATA: wage TYPE f,
          quan TYPE i.
ENDCLASS.



CLASS zcl_c5u2_pieceworker IMPLEMENTATION.
  METHOD constructor.
    CALL METHOD super->constructor
      EXPORTING
        firstname = fn
        lastname  = ln.
    wage = wg.
    quan = qu.
  ENDMETHOD.
  METHOD setwage.
    wage = wg.
  ENDMETHOD.
  METHOD setquan.
    quan = qu.
  ENDMETHOD.
  METHOD earnings.
    earn = wage * quan.
  ENDMETHOD.
  METHOD print.
    WRITE: /, 'My name is:', get_printname( ).
    WRITE: /, 'My salary is:', me->earnings( ) DECIMALS 2 EXPONENT 0.
  ENDMETHOD.
ENDCLASS.
