*&---------------------------------------------------------------------*
*& Program name: Z_FACTORY_METHOD
*& Description : Code Factory Method for Background/Foreground Execution
*&---------------------------------------------------------------------*
*& Created   By: GEORGE DRAKOS
*& Created   On: 04/05/2025
*&---------------------------------------------------------------------*
REPORT  z_factory_method.

*&----------------------------------------------------------------------*
*&CLASSES
*&----------------------------------------------------------------------*
INTERFACE lif_factory DEFERRED.

CLASS:
  "ABSTRACT
  lcl_handler            DEFINITION DEFERRED,
  "CONCRETE
  lcl_handler_foreground DEFINITION DEFERRED,
  lcl_handler_background DEFINITION DEFERRED.

*&----------------------------------------------------------------------*
*&CLASS LCX_EXCEPTION DEFINITION
*&----------------------------------------------------------------------*
CLASS lcx_exception DEFINITION INHERITING FROM cx_static_check FINAL CREATE PUBLIC.

  PUBLIC SECTION.

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

*&----------------------------------------------------------------------*
*&INTERFACE LIF_FACTORY
*&----------------------------------------------------------------------*
INTERFACE lif_factory.

  TYPES:BEGIN OF ENUM enum_execution_mode,
          foreground,
          background,
        END OF ENUM enum_execution_mode.

  CLASS-METHODS:

    factory IMPORTING im_execution_mode TYPE enum_execution_mode
            RETURNING VALUE(ro_obj)     TYPE REF TO lcl_handler
            RAISING   lcx_exception.


ENDINTERFACE.

*&----------------------------------------------------------------------*
*&CLASS ABSTRACT LCL_HANDLER DEFINITION
*&----------------------------------------------------------------------*
CLASS lcl_handler DEFINITION ABSTRACT.

  PUBLIC SECTION.

    INTERFACES lif_factory.

    ALIASES factory FOR lif_factory~factory.

    METHODS:
      execute ABSTRACT.

ENDCLASS.

*&----------------------------------------------------------------------*
*&CLASS LCL_HANDLER_FOREGROUND DEFINITION
*&----------------------------------------------------------------------*
CLASS lcl_handler_foreground DEFINITION INHERITING FROM lcl_handler.

  PUBLIC SECTION.

    METHODS execute REDEFINITION.

ENDCLASS.

*&----------------------------------------------------------------------*
*&CLASS LCL_HANDLER_BACKGROUND DEFINITION
*&----------------------------------------------------------------------*
CLASS lcl_handler_background DEFINITION INHERITING FROM lcl_handler.

  PUBLIC SECTION.

    METHODS execute REDEFINITION.

ENDCLASS.


*&----------------------------------------------------------------------*
*& SELECTION SCREEN DESIGN
*&----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

  PARAMETERS: p_back RADIOBUTTON GROUP rbg1 USER-COMMAND uc1 DEFAULT 'X',
              p_for  RADIOBUTTON GROUP rbg1.

SELECTION-SCREEN END OF BLOCK b1.

*&----------------------------------------------------------------------*
*& INITIALIZATION OF SELECTION SCREEN ELEMENTS
*&----------------------------------------------------------------------*
INITIALIZATION.
  %_p_back_%_app_%-text = 'Background Execution'.
  %_p_for_%_app_%-text  = 'Foreground Execution'.

*&----------------------------------------------------------------------*
*& EXECUTABLE CODE
*&----------------------------------------------------------------------*
START-OF-SELECTION.

  TRY.
      TRY.
          DATA(lo_execution) = CAST lcl_handler( lcl_handler=>factory( COND #( WHEN p_back EQ abap_true THEN lif_factory=>background
                                                                               WHEN p_for  EQ abap_true THEN lif_factory=>foreground
                                                                               ELSE THROW lcx_exception( im_text = 'Not Implemented') ) ) ).
          lo_execution->execute( ).
        CLEANUP.
          CLEAR:lo_execution.
      ENDTRY.
    CATCH lcx_exception INTO DATA(lo_exception).
      MESSAGE lo_exception->get_text( ) TYPE cl_cms_common=>con_msg_typ_i DISPLAY LIKE cl_cms_common=>con_msg_typ_e.
  ENDTRY.

END-OF-SELECTION.
*&----------------------------------------------------------------------*
*& END OF EXECUTABLE CODE
*&----------------------------------------------------------------------*

*&----------------------------------------------------------------------*
*&CLASS lcl_handler IMPLEMENTATION
*&----------------------------------------------------------------------*
CLASS lcl_handler IMPLEMENTATION.

  METHOD factory.

    ro_obj = SWITCH #( im_execution_mode
                       WHEN lif_factory=>background THEN NEW lcl_handler_background( )
                       WHEN lif_factory=>foreground THEN NEW lcl_handler_foreground( )
                       ELSE THROW lcx_exception( im_text = 'No Relevant Execution Class Found'  ) ).

  ENDMETHOD.

ENDCLASS.

*&----------------------------------------------------------------------*
*&CLASS LCL_HANDLER_FOREGROUND IMPLEMENTATION
*&----------------------------------------------------------------------*
CLASS lcl_handler_foreground IMPLEMENTATION.

  METHOD execute.
    WRITE: / 'Processing in Foreground'.
  ENDMETHOD.

ENDCLASS.

*&----------------------------------------------------------------------*
*&CLASS LCL_HANDLER_BACKGROUND IMPLEMENTATION
*&----------------------------------------------------------------------*
CLASS lcl_handler_background IMPLEMENTATION.

  METHOD execute.
    WRITE: / 'Processing in Background'.
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
