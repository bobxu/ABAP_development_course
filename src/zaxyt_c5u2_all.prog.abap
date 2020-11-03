*&---------------------------------------------------------------------*
*& Report zaxyt_c5u2_all
*&---------------------------------------------------------------------*
*&  课程：ABAP 开发入门
*&  章节：第五单元第二课时
*&  作者：A_216 @ Aug 20, 2018
*&---------------------------------------------------------------------*
REPORT zaxyt_c5u2_all.

data: o_sales type ref to zcl_c5u2_sales.
create OBJECT o_sales
  EXPORTING
    fn   = 'Peter'
    ln   = 'ABC'
    sal  = '1234.56'
    comm = '0.2'
    quan = 120000
  .

o_sales->print( ).

data: o_emp type ref to zcl_c5u2_employee,
      o_boss type ref to zcl_c5u2_boss.

create OBJECT o_boss
  EXPORTING
    fn = 'Bob'
    ln = 'Xu'
    wl = '123.45'
  .

o_emp = o_boss.

o_emp->print( ).
