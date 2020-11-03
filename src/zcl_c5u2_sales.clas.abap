CLASS zcl_c5u2_sales DEFINITION
  PUBLIC
  INHERITING FROM zcl_c5u2_employee
  FINAL
  CREATE PUBLIC .

PUBLIC SECTION.
    METHODS:
        constructor IMPORTING fn TYPE string
            ln TYPE string
            sal TYPE float
            comm TYPE float
            quan TYPE i,
       setsalary IMPORTING sal TYPE f,
       setcomm IMPORTING comm TYPE f,
       setquantity IMPORTING qu TYPE i,
       earnings REDEFINITION,
       print REDEFINITION.
PROTECTED SECTION.
PRIVATE SECTION.
    DATA: salary TYPE f,
          commission TYPE f,
          quantity TYPE i.
ENDCLASS.



CLASS zcl_c5u2_sales IMPLEMENTATION.
    METHOD constructor.
        CALL METHOD super->constructor
          EXPORTING
            firstname = fn
            lastname  = ln
          .
          salary = sal.
          commission = comm.
          quantity = quan.
    ENDMETHOD.
    METHOD setsalary.
    salary = sal.
    ENDMETHOD.
    METHOD setcomm.
        commission = comm.
    ENDMETHOD.
    METHOD setquantity.
        quantity = qu.
    ENDMETHOD.
    METHOD earnings.
        earn = salary + commission * quantity.
    ENDMETHOD.
    METHOD print.
    WRITE: /, 'My name is:', get_printname( ).
    WRITE: /, 'My salary is:', me->earnings( ) DECIMALS 2 EXPONENT 0.
    ENDMETHOD.
ENDCLASS.
