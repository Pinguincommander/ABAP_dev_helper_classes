class ZCL_STACK definition
  public
  final
  create public .

public section.
*"* public components of class ZCL_STACK
*"* do not include other source files here!!!

  methods INSERT
    importing
      !IW_DATA type ANY
      !IW_IDX type I .
  methods APPEND
    importing
      !IW_DATA type ANY .
  methods EXTEND .
  methods COUNT
    importing
      !IW_DATA type ANY
    returning
      value(RW_COUNT) type I .
  methods INDEX
    importing
      !IW_DATA type ANY
    returning
      value(RW_IDX) type I .
  methods POP
    importing
      !IW_IDX type I default 1
    exporting
      !EW_DATA type ANY .
  methods REMOVE
    importing
      !IW_DATA type ANY .
  methods REVERSE .
  methods SORT .
  methods CONSTRUCTOR
    importing
      !IT_DATA type ANY TABLE optional
      !IW_INDEX type I default 1
    exceptions
      INDEX_OUT_OF_BOUNDS .
  methods LENGTH
    returning
      value(RW_LENGTH) type I .
  PROTECTED SECTION.
*"* protected components of class ZCL_STACK
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_STACK
*"* do not include other source files here!!!

  data GT_STACK type ZKE_COM_T_STACK .
  data GS_STACK type ZKE_COM_S_STACK .
  data GR_STACK_OBJECT type ref to ZCL_STACK_OBJECT .
  data GW_LENGTH type I .

  type-pools ABAP .
  methods COMPARE_TYPE
    importing
      !IW_DATA1 type ANY
      !IW_DATA2 type ANY
    returning
      value(RW_EQUAL) type ABAP_BOOL .
ENDCLASS.



CLASS ZCL_STACK IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_STACK->APPEND
* +-------------------------------------------------------------------------------------------------+
* | [--->] IW_DATA                        TYPE        ANY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD APPEND.

*   entgegennehmen des Import Parameters
    gr_stack_object->set( iw_data ).

*   Anhängen des Eintrags an Tabelle.
    gs_stack-object ?= gr_stack_object->if_os_clone~clone( ).
    APPEND gs_stack TO gt_stack.

*   Länge anpassen.
    gw_length = gw_length + 1.

  ENDMETHOD.                    "APPEND


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_STACK->COMPARE_TYPE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IW_DATA1                       TYPE        ANY
* | [--->] IW_DATA2                       TYPE        ANY
* | [<-()] RW_EQUAL                       TYPE        ABAP_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD COMPARE_TYPE.
    DATA: lr_descr TYPE REF TO cl_abap_typedescr,
          lw_type1 TYPE abap_typekind,
          lw_type2 TYPE abap_typekind.

    rw_equal = abap_false.

    lw_type1 = cl_abap_typedescr=>describe_by_data( iw_data1 )->type_kind.
    lw_type2 = cl_abap_typedescr=>describe_by_data( iw_data2 )->type_kind.

    IF lw_type1 = lw_type2.
      rw_equal = abap_true.
    ENDIF.
  ENDMETHOD.                    "COMPARE_TYPE


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_STACK->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_DATA                        TYPE        ANY TABLE(optional)
* | [--->] IW_INDEX                       TYPE        I (default =1)
* | [EXC!] INDEX_OUT_OF_BOUNDS
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD CONSTRUCTOR.

*   Initialisieren der benötigten Objekte
    CREATE OBJECT gr_stack_object.
    gw_length = 0.

*   Übernehmen der mitgeflieferten Tabelle als werte.
    IF it_data IS NOT INITIAL.

*     erstellen der benötigten Variablen.
      DATA: lr_tabledescr  TYPE REF TO cl_abap_tabledescr,
            lr_structdescr TYPE REF TO cl_abap_structdescr,
            lt_field_list  TYPE abap_component_tab.
      FIELD-SYMBOLS: <is_data> TYPE any,
                     <iw_data> TYPE any,
                     <ls_field_list> TYPE LINE OF abap_component_tab.

*     Auslesen, welche Spalte zu Tabelle hinzugefügt werden soll
      lr_tabledescr  ?= cl_abap_tabledescr=>describe_by_data( it_data ).
      lr_structdescr ?= lr_tabledescr->get_table_line_type( ).
      lt_field_list = lr_structdescr->get_components( ).
      READ TABLE lt_field_list INDEX iw_index ASSIGNING <ls_field_list>.
      IF sy-subrc NE 0.
        RAISE index_out_of_bounds.
      ENDIF.

*     Lesen und anhängen der Tabelle.
      LOOP AT it_data ASSIGNING <is_data>.
        ASSIGN COMPONENT <ls_field_list>-name OF STRUCTURE <is_data> TO <iw_data>.
        me->append( <iw_data> ).
      ENDLOOP.

    ENDIF.

  ENDMETHOD.                    "CONSTRUCTOR


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_STACK->COUNT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IW_DATA                        TYPE        ANY
* | [<-()] RW_COUNT                       TYPE        I
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD COUNT.

*   Lokale variablen deklarieren
    FIELD-SYMBOLS: <ls_stack> TYPE z_s_stack,
                   <lw_data>  TYPE any.
    DATA:          lw_data    TYPE REF TO data.

*   Initialisieren der Daten
    rw_count = 0.

*   Schleife über Stack Tabelle und lesen der werte,
*   Wenn werte übereinstimmen, Zähler erhöhen.
    LOOP AT gt_stack ASSIGNING <ls_stack>.
      lw_data = <ls_stack>-object->get( ).
      ASSIGN lw_data->* TO <lw_data>.
*     Prüfen auf Typengleichheit bevor Daten verglichen werden.
      IF me->compare_type( iw_data1 = iw_data iw_data2 = <lw_data> ) = abap_true.
        IF <lw_data> EQ iw_data.
          rw_count = rw_count + 1.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.                    "COUNT


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_STACK->EXTEND
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD EXTEND.

  ENDMETHOD.                    "EXTEND


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_STACK->INDEX
* +-------------------------------------------------------------------------------------------------+
* | [--->] IW_DATA                        TYPE        ANY
* | [<-()] RW_IDX                         TYPE        I
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD INDEX.
    FIELD-SYMBOLS <lw_data> TYPE any.
    DATA: lw_tabix TYPE i,
          lw_data  TYPE REF TO data.

*   Lokale variablen deklarieren
    FIELD-SYMBOLS: <ls_stack> TYPE z_s_stack.

*   Initialisieren der Daten
    rw_idx = 0.

*   Schleife über Stack Tabelle und lesen der werte,
*   Wenn werte übereinstimmen, Zähler erhöhen.
    LOOP AT gt_stack ASSIGNING <ls_stack>.
      lw_tabix = sy-tabix.
      lw_data = <ls_stack>-object->get( ).
      ASSIGN lw_data->* TO <lw_data>.
      IF me->compare_type( iw_data1 = iw_data iw_data2 = <lw_data> ) = abap_true.
        IF <lw_data> = iw_data.
          rw_idx = lw_tabix.
          RETURN.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.                    "INDEX


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_STACK->INSERT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IW_DATA                        TYPE        ANY
* | [--->] IW_IDX                         TYPE        I
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD INSERT.
*   Prüfen ob Index Angabe in Ordnung ist.
    DATA lw_idx TYPE i.
    IF iw_idx > gw_length
    OR iw_idx <= 0.
      lw_idx = gw_length + 1.
    ENDIF.
*   Setzen von Objekt
    gr_stack_object->set( iw_data ).
*   Setzen der Struktur
    gs_stack-object ?= gr_stack_object->if_os_clone~clone( ).
*   Einfügen in Stack Tabelle.
    INSERT gs_stack INTO gt_stack INDEX lw_idx.
    gw_length = gw_length + 1.
  ENDMETHOD.                    "INSERT


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_STACK->LENGTH
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RW_LENGTH                      TYPE        I
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD LENGTH.
    rw_length = gw_length.
  ENDMETHOD.                    "LENGTH


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_STACK->POP
* +-------------------------------------------------------------------------------------------------+
* | [--->] IW_IDX                         TYPE        I (default =1)
* | [<---] EW_DATA                        TYPE        ANY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD POP.

    DATA: lw_value TYPE REF TO data,
          lw_idx   TYPE i.
    FIELD-SYMBOLS: <gs_stack> TYPE z_s_stack,
                   <fs> TYPE any.


*   Lesen der gewünschten Zeile.
    READ TABLE gt_stack INDEX iw_idx ASSIGNING <gs_stack>.
    IF sy-subrc = 0.
*     Lesen des Wertes und Ausgeben in Exportparameter vom Typ Any.
      lw_value = <gs_stack>-object->get( ).
      ASSIGN lw_value->* TO <fs>.
      ew_data = <fs>.
*     Löschen der gelesenen Zeile.
      DELETE gt_stack INDEX iw_idx.
      gw_length = gw_length - 1.
    ENDIF.

  ENDMETHOD.                    "pop


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_STACK->REMOVE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IW_DATA                        TYPE        ANY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD REMOVE.

    FIELD-SYMBOLS: <gs_stack> TYPE z_s_stack,
                   <lw_data>  TYPE any.
    DATA:          lw_data    TYPE REF TO data,
                   lv_tabix   TYPE i.

    LOOP AT gt_stack ASSIGNING <gs_stack>.
      lv_tabix = sy-tabix.
      lw_data = <gs_stack>-object->get( ).
      ASSIGN lw_data->* TO <lw_data>.
      IF me->compare_type( iw_data1 = iw_data iw_data2 = <lw_data> ) = abap_true.
        IF <lw_data> = iw_data.
          DELETE gt_stack INDEX lv_tabix.
          gw_length = gw_length - 1.
          RETURN.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.                    "REMOVE


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_STACK->REVERSE
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD REVERSE.

    DATA: lw_range TYPE i.
    FIELD-SYMBOLS: <ls_stack_top> TYPE z_s_stack,
                   <ls_stack_bot> TYPE z_s_stack.

*   Über Ganzzahlige Division die Hälft herausfinden(somit wird bei
*   ungerader anzahl der mittlere eintrag einfachen stehen gelassen).
    lw_range = gw_length DIV 2.

*   Schleife über die Hälft aller Einträge, und diese direkt tauschen.
    LOOP AT gt_stack ASSIGNING <ls_stack_top> TO lw_range.
*     Lesen des unteren wert mit dem getauscht werdne soll.
      READ TABLE gt_stack INDEX ( gw_length - sy-tabix ) ASSIGNING <ls_stack_bot>.
*     Zwischenspeichern des oberen wert.
      gs_stack       = <ls_stack_top>.
*     Überschreiben des oberen Wert mit unterem Wert.
      <ls_stack_top> = <ls_stack_bot>.
*     Überschreiben des unteren wert mit zwischengespeicherem oberen wert.
      <ls_stack_bot> = gs_stack.
    ENDLOOP.


  ENDMETHOD.                    "REVERSE


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_STACK->SORT
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD SORT.
    FIELD-SYMBOLS: <ls_stack1> TYPE z_s_stack,
                   <ls_stack2> TYPE z_s_stack,
                   <lw_value1> TYPE any,
                   <lw_value2> TYPE any.

    DATA:           lw_counter TYPE i,
                    lw_value1  TYPE REF TO data,
                    lw_value2  TYPE REF TO data,
                    lw_string1 TYPE string,
                    lw_string2 TYPE string.
    lw_counter = 1.

    DO gw_length TIMES.
      LOOP AT gt_stack ASSIGNING <ls_stack1> FROM lw_counter.
        IF ( ( sy-tabix + 1 ) <= gw_length ).
          READ TABLE gt_stack INDEX sy-tabix + 1 ASSIGNING <ls_stack2>.

          lw_value1 = <ls_stack1>-object->get( ).
          ASSIGN lw_value1->* TO <lw_value1>.
          lw_value2 = <ls_stack2>-object->get( ).
          ASSIGN lw_value2->* TO <lw_value2>.

          MOVE <lw_value1> TO lw_string1.
          MOVE <lw_value2> TO lw_string2.

          IF lw_string1 > lw_string2.
            gs_stack    = <ls_stack1>.
            <ls_stack1> = <ls_stack2>.
            <ls_stack2> = gs_stack.
          ENDIF.
        ENDIF.
      ENDLOOP.
      lw_counter = lw_counter + 1.
    ENDDO.

  ENDMETHOD.                    "SORT
ENDCLASS.