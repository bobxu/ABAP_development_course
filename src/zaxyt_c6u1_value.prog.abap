*&---------------------------------------------------------------------*
*& Report zaxyt_c6u1_value
*&---------------------------------------------------------------------*
*&  课程：ABAP 开发入门
*&  章节：第六单元第一课时
*&  作者：A_216 @ Aug 20, 2018
*&---------------------------------------------------------------------*
REPORT zaxyt_c6u1_value.
TYPES:BEGIN OF ty_tab,
        col1 TYPE string,
        col2 TYPE string,
      END OF ty_tab.
DATA: it_tab TYPE TABLE OF ty_tab.

DATA(wa_tab) = VALUE ty_tab( col1 = '10' col2 = '20' ).
cl_demo_output=>display( wa_tab ).

it_tab = VALUE #(
  (  col1 = '11' col2 = '21' )
  (  col1 = '12' col2 = '22' ) ).

cl_demo_output=>display(  it_tab[] ).
