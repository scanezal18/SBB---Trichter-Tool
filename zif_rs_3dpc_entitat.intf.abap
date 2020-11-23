interface ZIF_RS_3DPC_ENTITAT
*----------------------------------------------------------------------*
* SBB AG, SAP CCC-IT, Bern/Berne/Berna
*----------------------------------------------------------------------*
*
*-----------Änderungsdokumentation / documentation of changes----------*
* Version SAP-Rel Datum Bearbeiter
* Beschreibung Get user list for project
*----------------------------------------------------------------------*
* >000< 7.50 tt.mm.jjjj Firma(DXC Technology) / Name(Sigried Canezal)
* Programmerstellung / creation of program
*----------------------------------------------------------------------*
* >001< 7.50 tt.mm.jjjj Firma(company) / Name(name)
* ...
*----------------------------------------------------------------------*
  public .


  interfaces ZIF_RS_3DPC_ANTWORT .

  aliases GET_RESPONSE_QUERY
    for ZIF_RS_3DPC_ANTWORT~GET_RESPONSE_QUERY .
  aliases RESPONSE_DELETE
    for ZIF_RS_3DPC_ANTWORT~GET_RESPONSE_DELETE .
  aliases RESPONSE_POST
    for ZIF_RS_3DPC_ANTWORT~GET_RESPONSE_POST .
  aliases RESPONSE_POST_DEEP_ENTITY
    for ZIF_RS_3DPC_ANTWORT~GET_RESPONSE_POST_DEEP_ENTITY .
  aliases RESPONSE_QUERY_EXPAND
    for ZIF_RS_3DPC_ANTWORT~GET_RESPONSE_QUERY_EXPAND .
  aliases RESPONSE_READ
    for ZIF_RS_3DPC_ANTWORT~GET_RESPONSE_READ .
  aliases RESPONSE_READ_EXPAND
    for ZIF_RS_3DPC_ANTWORT~GET_RESPONSE_READ_EXPAND .
  aliases RESPONSE_UPDATE
    for ZIF_RS_3DPC_ANTWORT~GET_RESPONSE_UPDATE .

  methods POST
    returning
      value(RV_IF_RESPONSE) type ref to ZIF_RS_3DPC_ANTWORT
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods UPDATE
    returning
      value(RV_IF_RESPONSE) type ref to ZIF_RS_3DPC_ANTWORT
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods DELETE
    returning
      value(RV_IF_RESPONSE) type ref to ZIF_RS_3DPC_ANTWORT
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods READ
    returning
      value(RV_IF_RESPONSE) type ref to ZIF_RS_3DPC_ANTWORT
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods QUERY
    returning
      value(RV_IF_RESPONSE) type ref to ZIF_RS_3DPC_ANTWORT
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods READ_EXPAND
    returning
      value(RV_IF_RESPONSE) type ref to ZIF_RS_3DPC_ANTWORT
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods QUERY_EXPAND
    returning
      value(RV_IF_RESPONSE) type ref to ZIF_RS_3DPC_ANTWORT
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods POST_DEEP_ENTITY
    returning
      value(RV_IF_RESPONSE) type ref to ZIF_RS_3DPC_ANTWORT
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
endinterface.
