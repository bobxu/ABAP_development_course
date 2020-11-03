*&---------------------------------------------------------------------*
*& Report zaxyt_c2u4_all
*&---------------------------------------------------------------------*
*&  课程：ABAP 开发入门
*&  章节：第二单元第四课时
*&  作者：A_216 @ Aug 20, 2018
*&---------------------------------------------------------------------*
REPORT zaxyt_c2u4_all.

TYPES: BEGIN OF ty_student,
         name      TYPE char10,
         number(6) TYPE n,
         mark      TYPE i,
         height    TYPE p DECIMALS 2,
         unit      TYPE unit3,
         level     TYPE char10,
       END OF ty_student.
DATA: wa_student TYPE ty_student,
      it_student TYPE TABLE OF ty_student.
FIELD-SYMBOLS: <fs> TYPE ty_student.

wa_student-name     = 'John'.
wa_student-number   = 1.
wa_student-mark     = 68.
wa_student-height   = '1.46'.
wa_student-unit     = 'm'.
APPEND wa_student TO it_student.
wa_student-name     = 'Peter'.
wa_student-number   = 2.
wa_student-mark     = 45.
wa_student-height   = '1.56'.
wa_student-unit     = 'm'.
APPEND wa_student TO it_student.
wa_student-name     = 'Tom'.
wa_student-number   = 3.
wa_student-mark     = 23.
wa_student-height   = '1.34'.
wa_student-unit     = 'm'.
APPEND wa_student TO it_student.
wa_student-name     = 'Kevin'.
wa_student-number   = 4.
wa_student-mark     = 87.
wa_student-height   = '1.76'.
wa_student-unit     = 'm'.
APPEND wa_student TO it_student.

LOOP AT it_student INTO wa_student.
  WRITE: /, wa_student-name   ,
      wa_student-number,
      wa_student-mark  ,
      wa_student-height,
      wa_student-unit  .
ENDLOOP.

WRITE: /, '名字中带有 o 的'.
LOOP AT it_student INTO wa_student.
  IF wa_student-name CS 'o'.

    WRITE: /, wa_student-name   ,
          wa_student-number,
          wa_student-mark  ,
          wa_student-height,
          wa_student-unit  .
  ENDIF.
ENDLOOP.

WRITE: /, '成绩评级并更新表'.
LOOP AT it_student ASSIGNING <fs>.
  CASE <fs>-mark / 20.
    WHEN 1 OR 2 .
      <fs>-level = '差'.
    WHEN 3.
      <fs>-level = '还行'.
    WHEN 4 OR 5.
      <fs>-level = '优秀'.
    WHEN OTHERS.
      <fs>-level = '没有'.
  ENDCASE.
ENDLOOP.
LOOP AT it_student INTO wa_student.
  WRITE: /, wa_student-name   ,
      wa_student-number,
      wa_student-mark  ,
      wa_student-height,
      wa_student-unit  ,
      wa_student-level.
ENDLOOP.

DATA: avg   TYPE p DECIMALS 2,
      total TYPE i.
DO lines( it_student ) TIMES.
  READ TABLE it_student INTO wa_student INDEX sy-index.
  total = total + wa_student-mark.
ENDDO.

WRITE: /, '取得系统异常'.
DATA: cx_ref TYPE REF TO cx_sy_arithmetic_error.
TRY.
    avg = total / 0.
  CATCH cx_sy_arithmetic_error INTO cx_ref.
    WRITE: cx_ref->get_text( ).
ENDTRY.

WRITE: /, '班级平均成绩'.
avg = total / lines( it_student ).
WRITE: /, avg.
