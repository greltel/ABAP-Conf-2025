*&---------------------------------------------------------------------*
*& Program name: Z_ADAPTER
*& Description : Adapter Design Pattern
*&---------------------------------------------------------------------*
*& Created   By: GEORGE DRAKOS
*& Created   On: 4/6/2025
*&---------------------------------------------------------------------*
REPORT z_adapter.

*&---------------------------------------------------------------------*
*& Common Interface
*&---------------------------------------------------------------------*
INTERFACE lif_service_execution.

  TYPES:BEGIN OF t_changes,
          vendor TYPE ekko-lifnr,
        END OF t_changes.

  METHODS:change_data IMPORTING it_changes TYPE t_changes OPTIONAL.

ENDINTERFACE.

*----------------------------------------------------------------------*
* CLASS LCL_ARIBA_ADAPTER DEFINITION
*----------------------------------------------------------------------*
* Ariba Adapter
*----------------------------------------------------------------------*
CLASS lcl_ariba_adapter DEFINITION.

  PUBLIC SECTION.

    INTERFACES:lif_service_execution.

ENDCLASS.

*----------------------------------------------------------------------*
* CLASS LCL_SOAP_ADAPTER DEFINITION
*----------------------------------------------------------------------*
* SOAP Adapter
*----------------------------------------------------------------------*
CLASS lcl_soap_adapter DEFINITION.

  PUBLIC SECTION.

    INTERFACES:lif_service_execution.

ENDCLASS.

*----------------------------------------------------------------------*
* CLASS LCL_LEGACY_ADAPTER DEFINITION
*----------------------------------------------------------------------*
* Legacy Adapter
*----------------------------------------------------------------------*
CLASS lcl_legacy_adapter DEFINITION.

  PUBLIC SECTION.

    INTERFACES:lif_service_execution.

ENDCLASS.

*&---------------------------------------------------------------------*
*& EXECUTABLE CODE
*&---------------------------------------------------------------------*
START-OF-SELECTION.

  DATA lo_adapter TYPE REF TO lif_service_execution.

  lo_adapter = NEW lcl_legacy_adapter( ).
  lo_adapter->change_data( ).

  "Using the same interface we get new functionality
  lo_adapter = NEW lcl_soap_adapter( ).
  lo_adapter->change_data( ).

  lo_adapter = NEW lcl_ariba_adapter( ).
  lo_adapter->change_data( ).

END-OF-SELECTION.
*&---------------------------------------------------------------------*
*& END OF EXECUTABLE CODE
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
* CLASS LCL_ARIBA_ADAPTER IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_ariba_adapter IMPLEMENTATION.

  METHOD lif_service_execution~change_data.
    write:/ 'Ariba Adapter'.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* CLASS LCL_COUPA_ADAPTER IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_soap_adapter IMPLEMENTATION.

  METHOD lif_service_execution~change_data.
    write:/ 'SOAP Adapter'.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* CLASS LCL_LEGACY_ADAPTER IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_legacy_adapter IMPLEMENTATION.

  METHOD lif_service_execution~change_data.
    write:/ 'Legacy Adapter'.
  ENDMETHOD.

ENDCLASS.
