*&---------------------------------------------------------------------*
*& Program name: Z_MVC
*& Description : Model View Controller for Multiple Output Management
*&---------------------------------------------------------------------*
*& Created   By: GEORGE DRAKOS
*& Created   On: 4/6/2025
*&---------------------------------------------------------------------*
REPORT z_mvc.

*&---------------------------------------------------------------------*
*& Classes
*&---------------------------------------------------------------------*

"Abstract Classes
CLASS:lcl_view DEFINITION DEFERRED,
      lcl_model DEFINITION DEFERRED.

CLASS:
       "Controller Class
       lcl_controller     DEFINITION DEFERRED,
       "Model Implementation Classes
       lcl_model_order    DEFINITION DEFERRED,
       lcl_model_delivery DEFINITION DEFERRED,
       "View Implementation Classes
       lcl_view_salv      DEFINITION DEFERRED,
       lcl_view_smartform DEFINITION DEFERRED,
       lcl_view_pdf       DEFINITION DEFERRED.

*&---------------------------------------------------------------------*
*& SELECTION SCREEN DESIGN
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b0 WITH FRAME TITLE title0.

  PARAMETERS: r_ord RADIOBUTTON GROUP rbg1 DEFAULT 'X',
              r_del RADIOBUTTON GROUP rbg1.

  PARAMETERS: p_ord TYPE vbap-vbeln,
              p_del TYPE lips-vbeln.

SELECTION-SCREEN END OF BLOCK b0.

*----------------------------------------------------------------------*
* CLASS LCX_EXCEPTION DEFINITION
*----------------------------------------------------------------------*
* Exception Class
*----------------------------------------------------------------------*
CLASS lcx_exception DEFINITION INHERITING FROM cx_static_check FINAL CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES:
      if_t100_dyn_msg.

    ALIASES:
      msgv1 FOR  if_t100_dyn_msg~msgv1,
      msgv2 FOR  if_t100_dyn_msg~msgv2,
      msgv3 FOR  if_t100_dyn_msg~msgv3,
      msgv4 FOR  if_t100_dyn_msg~msgv4,
      msgty FOR  if_t100_dyn_msg~msgty.

    METHODS:
      constructor IMPORTING im_text     TYPE bapiret2-message OPTIONAL
                            im_textid   TYPE texid OPTIONAL
                            im_previous TYPE REF TO  cx_root OPTIONAL,
      get_text            REDEFINITION,
      get_longtext        REDEFINITION.

  PRIVATE SECTION.

    DATA:
         mv_message TYPE bapiret2-message.

ENDCLASS.

*----------------------------------------------------------------------*
* Abstract Class for Model
*----------------------------------------------------------------------*
* Model:Business Code-Logic-Data Retrieval
*----------------------------------------------------------------------*
CLASS lcl_model DEFINITION ABSTRACT FRIENDS lcl_view lcl_controller.

  PUBLIC SECTION.

    METHODS:
      set_selections ABSTRACT IMPORTING im_selections TYPE any OPTIONAL,
      fetch_data     ABSTRACT,
      return_data    ABSTRACT RETURNING VALUE(re_data) TYPE REF TO data.

  PROTECTED SECTION.

ENDCLASS.

*----------------------------------------------------------------------*
* CLASS LCL_MODEL_ORDER DEFINITION
*----------------------------------------------------------------------*
* Business Code-Logic-Data Retrieval
*----------------------------------------------------------------------*
CLASS lcl_model_order DEFINITION INHERITING FROM lcl_model FRIENDS lcl_view.

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_sel_options,
        s_sales_doc TYPE RANGE OF vbak-vbeln,
        "Fill Rest of Screen Select Options here
      END OF ty_sel_options,

      BEGIN OF ty_parameters,
        p_sales_doc TYPE vbak-vbeln,
        "Fill Rest of Screen Parameters here
      END OF ty_parameters,

      BEGIN OF ty_screen_values.

        INCLUDE TYPE ty_sel_options AS select_options.
        INCLUDE TYPE ty_parameters  AS parameters.

    TYPES:END OF ty_screen_values.

    "Redefine Abstract Class Methods and Add Respective Logic
    METHODS:
      set_selections REDEFINITION,
      fetch_data     REDEFINITION,
      return_data    REDEFINITION.

  PRIVATE SECTION."Add Extra Methods in either Private or Protected Section

    "PRIVATE DATA
    DATA:ls_screen_fields TYPE ty_screen_values,
         lt_table         TYPE STANDARD TABLE OF vbap.

ENDCLASS.

*----------------------------------------------------------------------*
* CLASS LCL_MODEL_DELIVERY DEFINITION
*----------------------------------------------------------------------*
* Business Code-Logic-Data Retrieval
*----------------------------------------------------------------------*
CLASS lcl_model_delivery DEFINITION INHERITING FROM lcl_model FRIENDS lcl_view.

  PUBLIC SECTION.

    TYPES:

      BEGIN OF ty_sel_options,
        s_sales_doc TYPE RANGE OF vbak-vbeln,
        "Fill Rest of Screen Select Options here
      END OF ty_sel_options,

      BEGIN OF ty_parameters,
        p_delivery TYPE likp-vbeln,
        "Fill Rest of Screen Parameters here
      END OF ty_parameters,

      BEGIN OF ty_screen_values.

        INCLUDE TYPE ty_sel_options AS select_options.
        INCLUDE TYPE ty_parameters  AS parameters.

    TYPES:END OF ty_screen_values.

    "Redefine Abstract Class Methods and Add Respective Logic
    METHODS:
      set_selections REDEFINITION,
      fetch_data     REDEFINITION,
      return_data    REDEFINITION.

  PROTECTED SECTION.

  PRIVATE SECTION."Add Extra Methods in either Private or Protected Section

    "PRIVATE DATA
    DATA:ls_screen_fields TYPE ty_screen_values,
         lt_table         TYPE STANDARD TABLE OF lips.


ENDCLASS.

*----------------------------------------------------------------------*
* CLASS lcl_controller DEFINITION
*----------------------------------------------------------------------*
* Forwards Requests between Model Layer/View Layer
*----------------------------------------------------------------------*
CLASS lcl_controller DEFINITION FINAL.

  PUBLIC SECTION.

    DATA:
      lo_model TYPE REF TO lcl_model,
      lo_view  TYPE REF TO lcl_view.

    METHODS:
      constructor IMPORTING im_model_name TYPE seoclsname
                            im_view_name  TYPE seoclsname
                  RAISING   lcx_exception.

ENDCLASS.

*----------------------------------------------------------------------*
* ABSTRACT CLASS FOR VIEW
*----------------------------------------------------------------------*
* View: Display Data(Data,ALV,Smartform,Adobe Form,PDF etc.)
*----------------------------------------------------------------------*
CLASS lcl_view DEFINITION ABSTRACT.

  PUBLIC SECTION.

    METHODS: display ABSTRACT IMPORTING im_controller_object TYPE REF TO lcl_controller.

ENDCLASS.

*----------------------------------------------------------------------*
* CLASS lcl_view_Salv DEFINITION
*----------------------------------------------------------------------*
* Display Using ALV
*----------------------------------------------------------------------*
CLASS lcl_view_salv DEFINITION INHERITING FROM lcl_view.

  PUBLIC SECTION.

    "Redefine Abstract Class Methods and Add Respective Logic
    METHODS: display REDEFINITION.

  PRIVATE SECTION."Add Extra Methods in either Private or Protected Section

    METHODS:build_field_catalog CHANGING co_salv TYPE REF TO cl_salv_table.

ENDCLASS.

*----------------------------------------------------------------------*
* CLASS lcl_view_smartform DEFINITION
*----------------------------------------------------------------------*
* Display in Smartform
*----------------------------------------------------------------------*
CLASS lcl_view_smartform DEFINITION INHERITING FROM lcl_view.

  PUBLIC SECTION.

    METHODS: display REDEFINITION.

ENDCLASS.

*----------------------------------------------------------------------*
* CLASS lcl_view_pdf DEFINITION
*----------------------------------------------------------------------*
* Display in PDF
*----------------------------------------------------------------------*
CLASS lcl_view_pdf DEFINITION INHERITING FROM lcl_view.

  PUBLIC SECTION.

    METHODS: display REDEFINITION.

ENDCLASS.

*&---------------------------------------------------------------------*
*& INITIALIZATION OF SELECTION SCREEN ELEMENTS
*&---------------------------------------------------------------------*
INITIALIZATION.
  title0               = 'Selection Screen Criteria'.
  %_r_ord_%_app_%-text = 'Order Model'.
  %_r_del_%_app_%-text = 'Delivery Model'.
  %_p_ord_%_app_%-text = 'Order Number'.
  %_p_del_%_app_%-text = 'Delivery Number'.

*&---------------------------------------------------------------------*
*& EXECUTABLE CODE
*&---------------------------------------------------------------------*
START-OF-SELECTION.

  TRY.
      "Make the Appropriate Selection for Model and View
      DATA(lo_controller) = NEW lcl_controller( im_model_name = COND #( WHEN r_ord IS NOT INITIAL THEN 'LCL_MODEL_ORDER'
                                                                        WHEN r_del IS NOT INITIAL THEN 'LCL_MODEL_DELIVERY'
                                                                        ELSE THROW lcx_exception( im_text = 'No Model Class Retrieved' )  )
                                                im_view_name  = 'lcl_view_Salv' ).

      "Execute logic Using Controller Object
      lo_controller->lo_model->set_selections( COND #( WHEN r_ord IS NOT INITIAL THEN VALUE lcl_model_order=>ty_screen_values( p_sales_doc = p_ord )
                                                       WHEN r_del IS NOT INITIAL THEN VALUE lcl_model_delivery=>ty_screen_values( p_delivery = p_del ) ) ).

      lo_controller->lo_model->fetch_data( ).

      lo_controller->lo_view->display( lo_controller ).

    CATCH lcx_exception INTO DATA(lo_exception).
      MESSAGE lo_exception->get_text( ) TYPE cl_cms_common=>con_msg_typ_i DISPLAY LIKE cl_cms_common=>con_msg_typ_e.
  ENDTRY.

END-OF-SELECTION.
*&---------------------------------------------------------------------*
*& END OF EXECUTABLE CODE
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
* CLASS lcl_controller IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_controller IMPLEMENTATION.

  METHOD constructor.

    DATA lv_object TYPE REF TO object.

    TRY.
        "Model Class
        DATA(lv_class_model) = condense( to_upper( im_model_name ) ).

        CREATE OBJECT lv_object TYPE (lv_class_model).
        IF syst-subrc IS INITIAL AND lv_object IS BOUND.
          lo_model = CAST #( lv_object ).
          CLEAR:lv_object.
        ENDIF.

        "View Class
        DATA(lv_class_view) = condense( to_upper( im_view_name ) ).

        CREATE OBJECT lv_object TYPE (lv_class_view).
        IF syst-subrc IS INITIAL AND lv_object IS BOUND.
          lo_view = CAST #( lv_object ).
          CLEAR:lv_object.
        ENDIF.

      CATCH cx_sy_create_object_error cx_sy_move_cast_error INTO DATA(lo_exception).
        RAISE EXCEPTION TYPE lcx_exception EXPORTING im_text = CONV #( lo_exception->get_text( ) ).
    ENDTRY.

  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* CLASS LCL_MODEL_ORDER IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_model_order IMPLEMENTATION.

  METHOD set_selections.

    me->ls_screen_fields = im_selections.

  ENDMETHOD.

  METHOD fetch_data.

    SELECT FROM vbap
    INNER JOIN vbak ON vbak~vbeln EQ vbap~vbeln
    FIELDS vbap~*
    WHERE ( vbak~vbeln EQ @ls_screen_fields-p_sales_doc )
    INTO TABLE @me->lt_table.

  ENDMETHOD.

  METHOD return_data.

    re_data = REF #( me->lt_table ).

  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* CLASS LCL_MODEL_DELIVERY IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_model_delivery IMPLEMENTATION.

  METHOD set_selections.

    me->ls_screen_fields = im_selections.

  ENDMETHOD.

  METHOD fetch_data.

    SELECT FROM lips
    INNER JOIN likp ON likp~vbeln EQ lips~vbeln
    FIELDS lips~*
    WHERE ( likp~vbeln EQ @ls_screen_fields-p_delivery )
    INTO TABLE @me->lt_table.

  ENDMETHOD.

  METHOD return_data.

    re_data = REF #( me->lt_table ).

  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_view_Salv IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_view_salv IMPLEMENTATION.

  METHOD display.

    FIELD-SYMBOLS:<fs_data> TYPE ANY TABLE.

    DATA(lt_table) = im_controller_object->lo_model->return_data( ).
    ASSIGN lt_table->* TO <fs_data>.

    TRY.
        cl_salv_table=>factory( IMPORTING r_salv_table = DATA(lo_alv)
                                CHANGING  t_table      = <fs_data> ).

        me->build_field_catalog( CHANGING co_salv = lo_alv ).

        lo_alv->display( ).

      CATCH cx_salv_msg cx_salv_no_new_data_allowed INTO DATA(lx_msg).
    ENDTRY.

  ENDMETHOD.

  METHOD build_field_catalog.


  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS LCL_VIEW_PDF IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_view_pdf IMPLEMENTATION.

  METHOD display.

  ENDMETHOD.

ENDCLASS.


*----------------------------------------------------------------------*
*       CLASS LCL_VIEW_SMARTFORM IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_view_smartform IMPLEMENTATION.

  METHOD display.

  ENDMETHOD.

ENDCLASS.

*&----------------------------------------------------------------------*
*&CLASS LCX_EXCEPTION IMPLEMENTATION
*&----------------------------------------------------------------------*
CLASS lcx_exception IMPLEMENTATION.

  METHOD constructor.

    super->constructor( textid = CONV #( im_textid )
                        previous = CONV #( im_previous ) ) ##OPERATOR[REFERENCE].

    mv_message = COND #( WHEN im_text IS SUPPLIED AND im_text IS NOT INITIAL THEN im_text ).

  ENDMETHOD.

  METHOD get_text.

    result = super->get_text( ).

    IF me->mv_message IS NOT INITIAL.
      result = COND #( WHEN result IS INITIAL THEN  me->mv_message
                       WHEN result IS NOT INITIAL THEN |{ result }-{ me->mv_message } | ).
    ENDIF.

  ENDMETHOD.

  METHOD get_longtext.

    result = super->get_longtext( ).

    IF me->mv_message IS NOT INITIAL.
      result = COND #( WHEN result IS INITIAL THEN  me->mv_message
                       WHEN result IS NOT INITIAL THEN |{ result }-{ me->mv_message } | ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.
