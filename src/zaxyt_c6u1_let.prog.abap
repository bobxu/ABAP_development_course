*&---------------------------------------------------------------------*
*& Report zaxyt_c6u1_let
*&---------------------------------------------------------------------*
*&  课程：ABAP 开发入门
*&  章节：第六单元第一课时
*&  作者：A_216 @ Aug 20, 2018
*&---------------------------------------------------------------------*
REPORT zaxyt_c6u1_let.

TYPES: BEGIN OF ty_name,
         first  TYPE string,
         middle TYPE string,
         last   TYPE string,
       END OF ty_name,
       ty_names TYPE TABLE OF ty_name WITH EMPTY KEY.

DATA(it_name) = VALUE ty_names(
  ( first = 'aa' middle = 'bb' last = 'cc' )
  ( first = 'dd' middle = 'ee' last = 'ff' )
).

DO lines( it_name ) TIMES.
  DATA(fullname) = CONV string(
    LET wa_name = it_name[ sy-index ]
      sep = '-'
    IN wa_name-first && sep && wa_name-middle && sep && wa_name-last
   ).
  WRITE: / fullname.
ENDDO.
