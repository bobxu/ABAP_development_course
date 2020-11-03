CLASS zcl_c5u2_employee DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

PUBLIC SECTION.
    METHODS:
* 构造器方法
      constructor IMPORTING firstname TYPE string
                            lastname  TYPE string,
*  取得全名
      get_printname RETURNING VALUE(fullname) TYPE string,
*  返回员工的收入
      earnings ABSTRACT RETURNING VALUE(earn) TYPE f,
*打印输出
      print ABSTRACT.
PROTECTED SECTION.
PRIVATE SECTION.
    DATA: first_name TYPE string,
          last_name  TYPE string.
ENDCLASS.



CLASS zcl_c5u2_employee IMPLEMENTATION.
  METHOD constructor.
    first_name = firstname.
    last_name = lastname.
  ENDMETHOD.

  METHOD get_printname.
    CONCATENATE last_name first_name INTO fullname SEPARATED BY space.
  ENDMETHOD.
ENDCLASS.
