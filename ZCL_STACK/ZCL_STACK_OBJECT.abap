class ZCL_STACK_OBJECT definition
  public
  create public .

public section.
*"* public components of class ZCL_STACK_OBJECT
*"* do not include other source files here!!!

  interfaces IF_OS_CLONE .

  methods CONSTRUCTOR
    importing
      !IW_DATA type ANY optional .
  methods SET
    importing
      !IW_DATA type ANY .
  methods GET
    returning
      value(RW_DATA) type ref to DATA .
protected section.
*"* protected components of class ZCL_STACK_OBJECT
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_STACK_OBJECT
*"* do not include other source files here!!!

  data GW_DATA type ref to DATA .
ENDCLASS.



CLASS ZCL_STACK_OBJECT IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_STACK_OBJECT->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IW_DATA                        TYPE        ANY(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
method CONSTRUCTOR.


endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_STACK_OBJECT->GET
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RW_DATA                        TYPE REF TO DATA
* +--------------------------------------------------------------------------------------</SIGNATURE>
method GET.
  rw_data = gw_data.
endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_STACK_OBJECT->IF_OS_CLONE~CLONE
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RESULT                         TYPE REF TO OBJECT
* +--------------------------------------------------------------------------------------</SIGNATURE>
method IF_OS_CLONE~CLONE.
   SYSTEM-CALL OBJMGR CLONE me TO result.
endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_STACK_OBJECT->SET
* +-------------------------------------------------------------------------------------------------+
* | [--->] IW_DATA                        TYPE        ANY
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD SET.

  FIELD-SYMBOLS: <data> type any.
  CREATE DATA gw_data like iw_data.
  ASSIGN gw_data->* TO <data>.
  <data> = iw_data.

ENDMETHOD.
ENDCLASS.