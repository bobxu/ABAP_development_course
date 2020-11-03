*&---------------------------------------------------------------------*
*& Report zaxyt_c2u3_string
*&---------------------------------------------------------------------*
*&  课程：ABAP 开发入门
*&  章节：第二单元第三课时
*&  作者：A_216 @ Aug 20, 2018
*&---------------------------------------------------------------------*
REPORT zaxyt_c2u3_string.

* 字符串：查找，格式化——shift，translate，正则表达式
*定义查找字符以及查找模式
DATA: ls(50) TYPE c VALUE 'abcd1234bcD234cd34'.
DATA: ls_f(2) TYPE c VALUE 'cd'.
*查找结果次数定义
DATA: li_c TYPE i.
*定义查找结果内表
DATA: li_tab TYPE match_result_tab,
      lwa    TYPE match_result.
*查找
FIND ALL OCCURRENCES OF ls_f
  IN ls
  IN CHARACTER MODE
  IGNORING CASE
  MATCH COUNT li_c
  RESULTS li_tab.
*查找结果输出
WRITE: li_c.
LOOP AT li_tab INTO lwa.
  WRITE: /, lwa-offset,lwa-length.
ENDLOOP.

* SHIFT
DATA: lc TYPE string.
lc = 'abcdefg'.
SHIFT lc LEFT DELETING LEADING 'ab'.
WRITE: /, lc.

* TRANSLATE
lc = '翱翔云天'.
TRANSLATE lc USING '翱你翔我'.
WRITE: /, lc.
