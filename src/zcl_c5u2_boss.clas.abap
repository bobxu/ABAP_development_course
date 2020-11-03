CLASS zcl_c5u2_boss DEFINITION
  PUBLIC
  INHERITING FROM zcl_c5u2_employee
  FINAL
  CREATE PUBLIC .

PUBLIC SECTION.
    METHODS:
        constructor IMPORTING fn TYPE string
            ln TYPE string
            wl TYPE f,
        set_weekly_salary IMPORTING ws TYPE f,
        earnings REDEFINITION,
        print REDEFINITION.
PROTECTED SECTION.
PRIVATE SECTION.
    DATA: weekly_salary TYPE f.
ENDCLASS.



CLASS zcl_c5u2_boss IMPLEMENTATION.
  METHOD constructor.
    CALL METHOD super->constructor
      EXPORTING
        firstname = fn
        lastname  = ln
      .
      weekly_salary = wl.
  ENDMETHOD.

  METHOD set_weekly_salary.
    weekly_salary = ws.
  ENDMETHOD.

  METHOD earnings.
    earn = weekly_salary * 4.
  ENDMETHOD.

  METHOD print.
    WRITE: /, 'My name is:', get_printname( ).
    WRITE: /, 'My salary is:', me->earnings( ) DECIMALS 2 EXPONENT 0.
  ENDMETHOD.
ENDCLASS.
