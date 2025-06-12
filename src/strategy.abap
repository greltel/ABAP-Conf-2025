*&---------------------------------------------------------------------*
*& Program name: Z_STRATEGY
*& Description : Strategy Design Pattern:open for extension but closed for modification
*&---------------------------------------------------------------------*
*& Created   By: GEORGE DRAKOS
*& Created   On: 04/05/2025
*&---------------------------------------------------------------------*
REPORT z_strategy.

*----------------------------------------------------------------------*
*INTERFACE if_strategy
*----------------------------------------------------------------------*
* ~Strategy:The strategy interface defines methods that represent common algorithms.
*  The context uses the strategy interface to call the algorithms implemented by ConcreteStrategy.
*----------------------------------------------------------------------*
INTERFACE if_strategy.

  METHODS process RETURNING VALUE(re_discount) TYPE vbap-netpr.

ENDINTERFACE.

*----------------------------------------------------------------------*
*  CLASS cl_app DEFINITION
*----------------------------------------------------------------------*
* ~Context:Context object contains a reference to a Strategy object.
*  The context object delegates requests from the client to the strategy object
*----------------------------------------------------------------------*
CLASS lcl_app DEFINITION.

  PUBLIC SECTION.

    METHODS:

      constructor IMPORTING im_extractable TYPE REF TO if_strategy,

      calculate_discount RETURNING VALUE(re_discount) TYPE vbap-netpr.

  PRIVATE SECTION.

    DATA mo_strategy TYPE REF TO if_strategy.

ENDCLASS.

*----------------------------------------------------------------------*
*  CLASS lcl_discount_customer_a DEFINITION
*----------------------------------------------------------------------*
* ~Concrete Class Strategy:Implements the algorithm defined in the strategy interface
*----------------------------------------------------------------------*
CLASS lcl_discount_customer_a DEFINITION.

  PUBLIC SECTION.

    INTERFACES if_strategy.

    ALIASES:process FOR if_strategy~process.

ENDCLASS.

*----------------------------------------------------------------------*
*  CLASS lcl_discount_customer_b DEFINITION
*----------------------------------------------------------------------*
* ~Concrete Class Strategy:Implements the algorithm defined in the strategy interface
*----------------------------------------------------------------------*
CLASS lcl_discount_customer_b DEFINITION.

  PUBLIC SECTION.

    INTERFACES if_strategy.

    ALIASES:process FOR if_strategy~process.

ENDCLASS.

*&---------------------------------------------------------------------*
*& SELECTION SCREEN DESIGN
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE title0.

  PARAMETERS: p_cus_a RADIOBUTTON GROUP gr1,
              p_cus_b RADIOBUTTON GROUP gr1.

SELECTION-SCREEN END OF BLOCK b1.

*&---------------------------------------------------------------------*
*& INITIALIZATION OF SELECTION SCREEN ELEMENTS
*&---------------------------------------------------------------------*
INITIALIZATION.
  title0                 = 'Selection Screen Criteria'.
  %_p_cus_a_%_app_%-text = 'Customer A'.
  %_p_cus_b_%_app_%-text = 'Customer B'.

*&---------------------------------------------------------------------*
*& EXECUTABLE CODE
*&---------------------------------------------------------------------*
START-OF-SELECTION.

  DATA(lv_discount) = NEW lcl_app( im_extractable = COND #( WHEN p_cus_a EQ abap_true THEN NEW lcl_discount_customer_a( )
                                                            WHEN p_cus_b EQ abap_true THEN NEW lcl_discount_customer_b( ) ) )->calculate_discount( ).


  BREAK-POINT.


END-OF-SELECTION.
*&---------------------------------------------------------------------*
*& END OF EXECUTABLE CODE
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS cl_app IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_app IMPLEMENTATION.

  METHOD constructor.
    mo_strategy = im_extractable.
  ENDMETHOD.

  METHOD calculate_discount.

    IF mo_strategy IS BOUND.
      re_discount = mo_strategy->process( ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_discount_customer_a IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_discount_customer_a IMPLEMENTATION.

  METHOD process.
    re_discount = 23.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_discount_customer_b IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_discount_customer_b IMPLEMENTATION.

  METHOD process.
    re_discount = 10.
  ENDMETHOD.

ENDCLASS.
