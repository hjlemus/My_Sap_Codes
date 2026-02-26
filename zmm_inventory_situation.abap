*&---------------------------------------------------------------------*
*& Report ZMM_INVENTORY_SITUATION
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZMM_INVENTORY_SITUATION.

INCLUDE: ZMM_INVENTORY_SITUATION_TOP,
         ZMM_INVENTORY_SITUATION_SEL,
         ZMM_INVENTORY_SITUATION_MAIN,
         ZMM_INVENTORY_SITUATION_F01.

*&---------------------------------------------------------------------*
*& Include          ZMM_INVENTORY_SITUATION_TOP
*&---------------------------------------------------------------------*


TABLES: mbew, mara, makt, marm, mard, maw1, t001l, ekko, ekpo, eket,
        vbap, vbep, lips, tvkol, konp, a073, t001w, tvko, tvtw, tvkot.

CONSTANTS:
  c_kappl TYPE kappl VALUE 'V',      "Aplicación Ventas
  c_kschl TYPE kschl VALUE 'VKP0'.   "Condición de Precio Regular VKP0

DATA gv_langu TYPE sylangu.

TYPES:
  BEGIN OF ty_out,
    matnr       TYPE matnr,       "Reference
    ean11       TYPE ean11,       "UPC Code
    maktx       TYPE maktx,       "Description
    alt_uom_qty TYPE i,     "Pk
    base_unit   TYPE meins,       "Bum
    lgort       TYPE lgort_d,     "Storage Loc
    qty_on_hand TYPE i,     "Quantity On Hand
    po_qty      TYPE i,     "PO Qty
    sto_qty     TYPE i,     "STO Qty
    so_qty      TYPE i,     "Sales Order Qty
    deliv_qty   TYPE i,     "Deliveries sin PGI
    qty_avail   TYPE i,     "Qty Available
    cost        TYPE verpr,       "Cost (MBEW-VERPR)
    total_cost  TYPE wrbtr,       "Qty_avail * cost
    reg_price   TYPE konp-kbetr,  "Regular Price
    total_sales TYPE wrbtr,       "Qty_avail * reg_price
    raube       TYPE raube,
  END OF ty_out.

DATA:
  gt_out_base TYPE STANDARD TABLE OF ty_out WITH DEFAULT KEY,
  gt_out      TYPE STANDARD TABLE OF ty_out WITH DEFAULT KEY.

DATA:
  gs_out   TYPE ty_out,
  gv_matnr TYPE matnr,
  gv_werks TYPE werks_d.

DATA:
  gs_mrp_detail TYPE bapi_mrp_stock_detail,
  gv_mrp_list   TYPE bapi_mrp_list,
  gv_mrp_area   TYPE bapi_mrp_mat_param-mrp_area,
  gs_control    TYPE bapi_mrp_control_param.

DATA:
  gt_mrp_items       TYPE STANDARD TABLE OF bapi_mrp_items,
  gt_mrp_ind_lines   TYPE STANDARD TABLE OF bapi_mrp_ind_lines,
  gt_mrp_total_lines TYPE STANDARD TABLE OF bapi_mrp_total_lines.

TYPES:
  BEGIN OF ty_knumh,
    knumh TYPE knumh,
  END OF ty_knumh.

DATA:
  gt_knumh TYPE STANDARD TABLE OF ty_knumh WITH DEFAULT KEY,
  gs_knumh TYPE ty_knumh.

DATA:
  gs_konp TYPE konp.

TYPES:
  BEGIN OF ty_marm,
    matnr TYPE matnr,
    meinh TYPE meinh,
    umrez TYPE umrez,
    umren TYPE umren,
    ean11 TYPE ean11,
  END OF ty_marm.

DATA:
  gt_marm TYPE STANDARD TABLE OF ty_marm WITH DEFAULT KEY,
  gs_marm TYPE ty_marm.

DATA:
  gt_vbap TYPE STANDARD TABLE OF vbap,
  gt_vbep TYPE STANDARD TABLE OF vbep,
  gs_vbap TYPE vbap,
  gs_vbep TYPE vbep.

DATA:
  lv_po    TYPE menge_d,
  lv_sto   TYPE menge_d,
  lv_so    TYPE menge_d,
  lv_deliv TYPE menge_d.

DATA:
  gt_marm_hash TYPE HASHED TABLE OF ty_marm
      WITH UNIQUE KEY matnr meinh.

DATA:
  gt_return TYPE STANDARD TABLE OF bapiret2,
  gs_return TYPE bapiret2.


*&---------------------------------------------------------------------*
*& Include          ZMM_INVENTORY_SITUATION_SEL
*&---------------------------------------------------------------------*

"---------------------------------------------------------------
" Group 1: BD Selection
"---------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

  SELECT-OPTIONS:
    s_matnr FOR mara-matnr.           " Material (select-option)

  PARAMETERS:
    p_werks TYPE  werks_d OBLIGATORY DEFAULT 'CD01'.  "Centro

  SELECT-OPTIONS:
    s_lgort FOR   T001L-lgort NO INTERVALS.                             " Almacén (nuevo)

SELECTION-SCREEN END OF BLOCK b1.

"---------------------------------------------------------------
" Grupo 2: Alcance de la Lista
"---------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.

  SELECT-OPTIONS:
    s_mtart FOR mara-mtart,           " Tipo de material
    s_matkl FOR mara-matkl,           " Grupo de artículos
    s_wekgr FOR maw1-wekgr.           " Grupo de compras (Retail)

SELECTION-SCREEN END OF BLOCK b2.

"---------------------------------------------------------------
" Grupo 3: Determinación de Precio de Venta
"---------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003.

  PARAMETERS:
    p_prdat TYPE sy-datum DEFAULT sy-datum,            " Fecha de precio
    p_vkorg TYPE vkorg DEFAULT '1010',               " Org. de Ventas
    p_vtweg TYPE vtweg DEFAULT '10'.               " Canal

SELECTION-SCREEN END OF BLOCK b3.



"---------------------------------------------------------------
" Inicialización de defaults
"---------------------------------------------------------------

INITIALIZATION.

  gv_langu = sy-langu.   " Inicializar idioma global
  gv_mrp_area = p_werks. " Iniciarlizar para pasar a la BAPI

  "---------------------------------------------------------------
  " Validaciones generales (llaman a FORM en F01)
  "---------------------------------------------------------------

AT SELECTION-SCREEN.

  PERFORM validate_werks  USING p_werks.
  PERFORM validate_lgort  USING p_werks.
  PERFORM validate_vkorg  USING p_vkorg.
  PERFORM validate_vtweg  USING p_vtweg.


*&---------------------------------------------------------------------*
*& Include          ZMM_INVENTORY_SITUATION_MAIN
*&---------------------------------------------------------------------*

START-OF-SELECTION.

  "---------------------------------------------------------------*
  " 1) Paso 1 – Extracción Base (GT_OUT_BASE)
  "---------------------------------------------------------------*
  PERFORM get_base_data.

  "---------------------------------------------------------------*
  " 2) Paso 2 – Llamar BAPI (por cada material) + buckets MRP
  "---------------------------------------------------------------*
  PERFORM get_mrp_data.

  "---------------------------------------------------------------*
  " 3) Paso 5 – Mostrar ALV final
  "---------------------------------------------------------------*
  PERFORM build_alv.

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*& Include          ZMM_INVENTORY_SITUATION_F01
*&---------------------------------------------------------------------*

*---------------------------------------------------------------------*
*  VALIDACIONES DEL SELECTION-SCREEN
*---------------------------------------------------------------------*

FORM validate_werks  USING iv_werks TYPE werks_d.

  DATA lv_dummy_werks TYPE werks_d.

  SELECT SINGLE werks
    INTO @lv_dummy_werks
    FROM t001w
   WHERE werks = @iv_werks.

  IF sy-subrc <> 0.
    MESSAGE e398(00) WITH |Centro { iv_werks } no existe en T001W|.
  ENDIF.
ENDFORM.


FORM validate_lgort  USING    p_i_werks.
  " Si no ingresaron almacenes (rango vacío), no hay nada que validar.
  IF s_lgort[] IS INITIAL.
    RETURN.
  ENDIF.

  DATA: lt_bad TYPE STANDARD TABLE OF lgort_d WITH DEFAULT KEY,
        lv_lg  TYPE lgort_d.

  " 1) Rechazar cualquier cosa que no sea valor individual incluido
  LOOP AT s_lgort ASSIGNING FIELD-SYMBOL(<ls>).
    IF <ls>-sign   <> 'I' OR
       <ls>-option <> 'EQ'.
      MESSAGE e398(00) WITH
        'Para "Almacén" solo se permiten valores individuales (incluidos).'.
    ENDIF.
  ENDLOOP.

  " 2) Validar que cada LGORT exista en T001L para el WERKS indicado
  LOOP AT s_lgort ASSIGNING <ls>.
    SELECT SINGLE lgort
      FROM t001l
      INTO @lv_lg
      WHERE werks = @p_i_werks
        AND lgort = @<ls>-low.

    IF sy-subrc <> 0.
      APPEND <ls>-low TO lt_bad.
    ENDIF.
  ENDLOOP.

  " 3) Si hay alguno inválido, armar el mensaje con la lista de fallos
  IF lt_bad IS NOT INITIAL.
    DATA(lv_list) = ||.
    LOOP AT lt_bad INTO DATA(lv).
      IF lv_list IS INITIAL.
        lv_list = lv.
      ELSE.
        lv_list = lv_list && `, ` && lv.
      ENDIF.
    ENDLOOP.

    MESSAGE e398(00)
      WITH |Los siguientes almacenes no existen en el centro { p_i_werks }: { lv_list }|.
  ENDIF.

ENDFORM.


FORM validate_vkorg USING iv_vkorg TYPE vkorg.
  DATA lv_dummy_vkorg TYPE vkorg.

  SELECT SINGLE vkorg
    INTO @lv_dummy_vkorg
    FROM tvko
   WHERE vkorg = @iv_vkorg.

  IF sy-subrc <> 0.
    MESSAGE e398(00) WITH |Org. ventas { iv_vkorg } no existe en TVKO|.
  ENDIF.
ENDFORM.


FORM validate_vtweg USING iv_vtweg TYPE vtweg.

  DATA lv_dummy_vtweg TYPE vtweg.

  SELECT SINGLE vtweg
    INTO @lv_dummy_vtweg
    FROM tvtw
   WHERE vtweg = @iv_vtweg.

  IF sy-subrc <> 0.
    MESSAGE e398(00) WITH |Canal { iv_vtweg } no existe en TVTW|.
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
* FORM GET_BASE_DATA
*---------------------------------------------------------------------*
FORM get_base_data .

  CLEAR: gt_out_base, gs_out.

  " 1) Traer MARD ya filtrado por MARA (mtart/matkl) y por centro/stock y Almacen
  SELECT
      m~matnr,
      m~werks,
      m~lgort,
      m~labst,
      a~meins,
      a~mtart,
      a~matkl,
      a~raube,
      w~wekgr
    FROM  mard AS m
    INNER JOIN mara AS a
            ON a~matnr = m~matnr
    LEFT OUTER JOIN maw1 AS w
            ON w~matnr = m~matnr
    INTO TABLE @DATA(lt_mard)
    WHERE m~matnr IN @s_matnr
      AND m~werks = @p_werks
      AND m~lgort IN @s_lgort
      AND m~labst > 0.

  IF lt_mard IS INITIAL.
*    MESSAGE 'No records found for the selected criteria.' TYPE 'I'.
    RETURN.
  ENDIF.


  " 2) Aplicar filtros opcionales por MATERIAL, TIPO, GRUPO, GR COMPRAS
  IF s_matnr[] IS NOT INITIAL.
    DELETE lt_mard WHERE matnr NOT IN s_matnr.
  ENDIF.

  IF s_mtart[] IS NOT INITIAL.
    DELETE lt_mard WHERE mtart NOT IN s_mtart.
  ENDIF.

  IF s_matkl[] IS NOT INITIAL.
    DELETE lt_mard WHERE matkl NOT IN s_matkl.
  ENDIF.

  "---------------------------------------------------------------*
  " 3) Filtro por Grupo de Compras (MAW1-WEKGR),
  "---------------------------------------------------------------*
  IF s_wekgr[] IS NOT INITIAL.
    DELETE lt_mard WHERE wekgr NOT IN s_wekgr.
  ENDIF.

  IF lt_mard IS INITIAL.
*    MESSAGE 'No records found for the selected criteria.' TYPE 'I'.
    RETURN.
  ENDIF.

  "---------------------------------------------------------------*
  " 4. Cargar textos MAKT (idioma usuario con fallback)
  "---------------------------------------------------------------*
  SELECT matnr,
         maktx,
         spras
    FROM makt
    INTO TABLE @DATA(lt_makt)
    FOR ALL ENTRIES IN @lt_mard
    WHERE matnr = @lt_mard-matnr.

  "---------------------------------------------------------------*
  " 5. Cargar MARM (EAN + UM alternativas)
  "---------------------------------------------------------------*
  SELECT matnr,
         meinh,
         ean11,
         umrez,
         umren
    FROM marm
    INTO TABLE @DATA(lt_marm)
    FOR ALL ENTRIES IN @lt_mard
    WHERE matnr = @lt_mard-matnr.

  SORT lt_marm BY matnr meinh.

  "---------------------------------------------------------------*
  " X. Costos del Material
  "---------------------------------------------------------------*
  SELECT matnr,
         verpr
    FROM mbew
    INTO TABLE @DATA(lt_mbew)
    FOR ALL ENTRIES IN @lt_mard
    WHERE matnr = @lt_mard-matnr
    AND bwkey = @gv_mrp_area.
  "---------------------------------------------------------------*
  " 6. Armar GT_OUT_BASE
  "---------------------------------------------------------------*

  CLEAR gs_out.
  CLEAR gt_out_base.


  LOOP AT lt_mard ASSIGNING FIELD-SYMBOL(<ls>).

    CLEAR gs_out.
    " Reference
    gs_out-matnr       = <ls>-matnr.
    " Base Unit (MARA-MEINS)
    gs_out-base_unit   = <ls>-meins.
    " On Hand (LABST)
    gs_out-qty_on_hand = <ls>-labst.
    " Actualizar el Almacen
    gs_out-lgort = <ls>-lgort.
    " Actualizar Cond de Almac
    gs_out-raube = <ls>-raube.

    " Introducir COST
    READ TABLE lt_mbew ASSIGNING FIELD-SYMBOL(<ls_mbew_base>)
     WITH KEY matnr = <ls>-matnr.
    IF sy-subrc = 0.
      gs_out-cost = <ls_mbew_base>-verpr.
    ENDIF.

    " EAN (MEINH = MEINS)
    READ TABLE lt_marm ASSIGNING FIELD-SYMBOL(<ls_marm_base>)
         WITH KEY matnr = <ls>-matnr meinh = <ls>-meins
         BINARY SEARCH.
    IF sy-subrc = 0.
      gs_out-ean11 = <ls_marm_base>-ean11.
    ENDIF.

    " Pk: primera UM alt ≠ base; si no hay, 1
    DATA(lv_pk) = 1.
    LOOP AT lt_marm ASSIGNING FIELD-SYMBOL(<ls_marm_alt>)
         WHERE matnr = <ls>-matnr.
      IF <ls_marm_alt>-meinh <> <ls>-meins
         AND <ls_marm_alt>-umren IS NOT INITIAL.
        lv_pk = <ls_marm_alt>-umrez / <ls_marm_alt>-umren.
        EXIT.
      ENDIF.
    ENDLOOP.
    gs_out-alt_uom_qty = lv_pk.

    " Descripción por idioma con fallback
    READ TABLE lt_makt ASSIGNING FIELD-SYMBOL(<ls_makt>)
         WITH KEY matnr = <ls>-matnr spras = gv_langu.
    IF sy-subrc = 0.
      gs_out-maktx = <ls_makt>-maktx.
    ELSE.
      READ TABLE lt_makt ASSIGNING <ls_makt> WITH KEY matnr = <ls>-matnr.
      IF sy-subrc = 0.
        gs_out-maktx = <ls_makt>-maktx.
      ENDIF.
    ENDIF.

    APPEND gs_out TO gt_out_base.

  ENDLOOP.

ENDFORM.

*---------------------------------------------------------------------*
* FORM GET_MRP_DATA
*  Paso 2: Llamada a BAPI_MATERIAL_STOCK_REQ_LIST (1×1 por material)
*  y mapeo a buckets: PO_QTY / STO_QTY / SO_QTY / DELIV_QTY + QTY_AVAIL
*---------------------------------------------------------------------*
FORM get_mrp_data .

  "---------------------------------------------------------
  " Tipos auxiliares
  "---------------------------------------------------------
  TYPES: BEGIN OF ty_po_raw,
           matnr TYPE matnr,
           lgort TYPE lgort_d,
           meins TYPE meins,
           menge TYPE eket-menge,
           wemng TYPE eket-wemng,
           glmng TYPE eket-glmng,
         END OF ty_po_raw.

  TYPES: BEGIN OF ty_sto_raw,
           matnr TYPE matnr,
           meins TYPE meins,
           menge TYPE eket-menge,
           glmng TYPE eket-glmng,
         END OF ty_sto_raw.

  TYPES: BEGIN OF ty_deliv,
           matnr     TYPE matnr,
           lgort     TYPE lgort_d,
           deliv_qty TYPE menge_d,
         END OF ty_deliv.

  TYPES: BEGIN OF ty_so,
           matnr  TYPE matnr,
           so_qty TYPE menge_d,
         END OF ty_so.

  TYPES: BEGIN OF ty_price,
           matnr TYPE matnr,
           vrkme TYPE meins,
           kbetr TYPE kbetr,
         END OF ty_price.

  TYPES: BEGIN OF ty_uom,
           matnr TYPE matnr,
           meinh TYPE meins,
           umrez TYPE marm-umrez,
           umren TYPE marm-umren,
         END OF ty_uom.

  TYPES: BEGIN OF ty_tvkol,
           vstel TYPE tvkol-vstel,
           werks TYPE tvkol-werks,
           raube TYPE tvkol-raube,
           lgort TYPE tvkol-lgort,
         END OF ty_tvkol.

  "---------------------------------------------------------
  " Buffers
  "---------------------------------------------------------
  DATA: lt_po_raw  TYPE STANDARD TABLE OF ty_po_raw,
        lt_sto_raw TYPE STANDARD TABLE OF ty_sto_raw,
        lt_deliv   TYPE HASHED TABLE   OF ty_deliv  WITH UNIQUE KEY matnr lgort,
        lt_so      TYPE HASHED TABLE   OF ty_so     WITH UNIQUE KEY matnr,
        lt_price   TYPE HASHED TABLE   OF ty_price  WITH UNIQUE KEY matnr vrkme,
        lt_uom     TYPE HASHED TABLE   OF ty_uom    WITH UNIQUE KEY matnr meinh,
        lt_tvkol   TYPE HASHED TABLE   OF ty_tvkol  WITH UNIQUE KEY vstel werks raube.

  "---------------------------------------------------------
  " Lista de claves
  "---------------------------------------------------------
  DATA: lr_matnrs     TYPE RANGE OF matnr,
        lr_lgorts     TYPE RANGE OF lgort_d,
        lr_base_units TYPE RANGE OF meins,
        lr_raubes     TYPE RANGE OF raube.

  FIELD-SYMBOLS <ls_out> TYPE ty_out.

  LOOP AT gt_out_base ASSIGNING <ls_out>.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = <ls_out>-matnr ) TO lr_matnrs.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = <ls_out>-lgort ) TO lr_lgorts.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = <ls_out>-base_unit ) TO lr_base_units.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = <ls_out>-raube ) TO lr_raubes.
  ENDLOOP.

  SORT lr_matnrs BY low.
  DELETE ADJACENT DUPLICATES FROM lr_matnrs COMPARING low.

  SORT lr_lgorts BY low.
  DELETE ADJACENT DUPLICATES FROM lr_lgorts COMPARING low.

  SORT lr_base_units BY low.
  DELETE ADJACENT DUPLICATES FROM lr_base_units COMPARING low.

  SORT lr_raubes BY low.
  DELETE ADJACENT DUPLICATES FROM lr_raubes COMPARING low.

  "---------------------------------------------------------
  " 1) Precarga PO abiertas
  "---------------------------------------------------------
  SELECT ekpo~matnr,
         ekpo~lgort,
         ekpo~meins,
         eket~menge,
         eket~wemng,
         eket~glmng
    FROM ekko
    JOIN ekpo ON ekpo~ebeln = ekko~ebeln
    JOIN eket ON eket~ebeln = ekpo~ebeln
             AND eket~ebelp = ekpo~ebelp
   WHERE ekko~bstyp = 'F'
     AND ekko~bsart IN ('ZIMP','ZMER')
     AND ekko~loekz = ''
     AND ekpo~loekz = ''
     AND ekpo~elikz = ''
     AND ekpo~matnr IN @lr_matnrs
     AND ekpo~werks = @p_werks
     AND ekpo~lgort IN @lr_lgorts
     AND eket~menge > eket~wemng
   INTO TABLE @lt_po_raw.

  "---------------------------------------------------------
  " 2) Precarga STO abiertas
  "---------------------------------------------------------
  SELECT ekpo~matnr,
         ekpo~meins,
         eket~menge,
         eket~glmng
    FROM ekko
    JOIN ekpo ON ekpo~ebeln = ekko~ebeln
    JOIN eket ON eket~ebeln = ekpo~ebeln
             AND eket~ebelp = ekpo~ebelp
   WHERE ekko~bstyp = 'F'
     AND ekko~bsakz = 'T'
     AND ekko~reswk = @p_werks
     AND ekpo~matnr IN @lr_matnrs
     AND ekpo~loekz = ''
     AND eket~menge > eket~glmng
   INTO TABLE @lt_sto_raw.

  "---------------------------------------------------------
  " 3) Precarga entregas LIPS
  "---------------------------------------------------------
  SELECT matnr,
         lgort,
         SUM( lgmng ) AS deliv_qty
    FROM lips
   WHERE matnr IN @lr_matnrs
     AND lgort IN @lr_lgorts
     AND wbsta = 'A'
   GROUP BY matnr, lgort
   INTO TABLE @lt_deliv.

  "---------------------------------------------------------
  " 4) Precarga SO abiertas
  "---------------------------------------------------------
  SELECT a~matnr,
         SUM( v~ordqty_bu ) AS so_qty
    FROM vbap AS a
    JOIN vbep AS v
      ON v~vbeln = a~vbeln
     AND v~posnr = a~posnr
   WHERE a~matnr IN @lr_matnrs
     AND a~werks = @p_werks
     AND a~lfsta IN ('A','B')
   GROUP BY a~matnr
   INTO TABLE @lt_so.

  "---------------------------------------------------------
  " 5) Precarga precios VKP0
  "---------------------------------------------------------
  SELECT a~matnr,
         a~vrkme,
         k~kbetr
    FROM a073 AS a
    JOIN konp AS k
      ON k~knumh = a~knumh
     AND k~kappl = 'V'
     AND k~kschl = 'VKP0'
     AND k~loevm_ko = ''
   WHERE a~kappl = 'V'
     AND a~kschl = 'VKP0'
     AND a~vkorg = @p_vkorg
     AND a~vtweg = @p_vtweg
     AND a~matnr IN @lr_matnrs
     AND a~vrkme IN @lr_base_units
     AND a~datab <= @p_prdat
     AND a~datbi >= @p_prdat
   INTO TABLE @lt_price.

  "---------------------------------------------------------
  " 6) Precarga conversiones MARM
  "---------------------------------------------------------
  SELECT matnr,
         meinh,
         umrez,
         umren
    FROM marm
   WHERE matnr IN @lr_matnrs
     AND meinh IN @lr_base_units
   INTO TABLE @lt_uom.

  "---------------------------------------------------------
  " 7) Precarga conversiones TVKOL para el Alm Picking
  "---------------------------------------------------------
  SELECT vstel
         werks
         raube
         lgort
    FROM tvkol
    INTO TABLE lt_tvkol
    WHERE vstel = p_werks
      AND werks = p_werks.

  "=========================================================
  " LOOP PRINCIPAL — puro memory lookup (rápido)
  "=========================================================
  LOOP AT gt_out_base ASSIGNING <ls_out>.

    CLEAR: <ls_out>-po_qty,
           <ls_out>-sto_qty,
           <ls_out>-so_qty,
           <ls_out>-deliv_qty,
           <ls_out>-reg_price.

    "---------------------------
    " PO abiertas
    "---------------------------
    LOOP AT lt_po_raw INTO DATA(ls_po)
         WHERE matnr = <ls_out>-matnr
           AND lgort = <ls_out>-lgort.

      DATA(lv_open_po) = ls_po-menge - ls_po-wemng.
      DATA(lv_po_conv) = lv_open_po.

      READ TABLE lt_uom INTO DATA(ls_uom)
           WITH KEY matnr = <ls_out>-matnr meinh = ls_po-meins.

      IF sy-subrc = 0 AND ls_uom-umrez > 0 AND ls_uom-umren > 0.
        lv_po_conv = lv_open_po * ls_uom-umrez / ls_uom-umren.
      ENDIF.

      <ls_out>-po_qty += lv_po_conv.
    ENDLOOP.

    "---------------------------
    " STO — SOLO si el LGORT de la fila = LGORT de picking
    "           del RAUBE del propio material de la fila
    "           para el centro (p_werks)
    "---------------------------

    " Resolver el LGORT de picking del RAUBE de ESTA fila en ESTE centro
    READ TABLE lt_tvkol INTO DATA(ls_tv)
         WITH KEY vstel = p_werks
                  werks = p_werks
                  raube = <ls_out>-raube.
    IF sy-subrc = 0 AND ls_tv-lgort = <ls_out>-lgort.
      " Solo el almacén de picking recibe STO
      LOOP AT lt_sto_raw INTO DATA(ls_sto)
           WHERE matnr = <ls_out>-matnr.

        DATA(lv_open_sto) = ls_sto-menge - ls_sto-glmng.
        DATA(lv_sto_conv) = lv_open_sto.

        READ TABLE lt_uom INTO ls_uom
             WITH KEY matnr = <ls_out>-matnr meinh = ls_sto-meins.

        IF sy-subrc = 0 AND ls_uom-umrez > 0 AND ls_uom-umren > 0.
          lv_sto_conv = lv_open_sto * ls_uom-umrez / ls_uom-umren.
        ENDIF.
        <ls_out>-sto_qty += lv_sto_conv.
      ENDLOOP.
    ELSE.
      " No coincide con el almacén de picking del RAUBE → 0
      <ls_out>-sto_qty = 0.
    ENDIF.



    "---------------------------
    " Entregas LIPS
    "---------------------------
    READ TABLE lt_deliv INTO DATA(ls_deliv)
         WITH KEY matnr = <ls_out>-matnr
                  lgort = <ls_out>-lgort.
    IF sy-subrc = 0.
      <ls_out>-deliv_qty = ls_deliv-deliv_qty.
    ENDIF.

    "---------------------------
    " SO abiertas
    "---------------------------
    READ TABLE lt_so INTO DATA(ls_so)
         WITH KEY matnr = <ls_out>-matnr.
    IF sy-subrc = 0.
      <ls_out>-so_qty = ls_so-so_qty.
    ENDIF.

    "---------------------------
    " Precio VKP0
    "---------------------------
    READ TABLE lt_price INTO DATA(ls_price)
         WITH KEY matnr = <ls_out>-matnr
                  vrkme = <ls_out>-base_unit.
    IF sy-subrc = 0.
      <ls_out>-reg_price = ls_price-kbetr.
    ENDIF.

    "---------------------------
    " Cálculos finales
    "---------------------------
    <ls_out>-qty_avail   = <ls_out>-qty_on_hand - <ls_out>-deliv_qty.
    <ls_out>-total_cost  = <ls_out>-qty_avail * <ls_out>-cost.
    <ls_out>-total_sales = <ls_out>-qty_avail * <ls_out>-reg_price.

  ENDLOOP.

ENDFORM.

*---------------------------------------------------------------------*
* Devuelve nombre de Org. Ventas (TVKOT) según idioma de usuario
*---------------------------------------------------------------------*
FORM get_vkorg_name USING    iv_vkorg    TYPE vkorg
                    CHANGING cv_vkorg_tx TYPE tvkot-vtext.

  CLEAR cv_vkorg_tx.
  SELECT SINGLE vtext
    INTO @cv_vkorg_tx
    FROM tvkot
   WHERE vkorg = @iv_vkorg
     AND spras = @gv_langu.         " gv_langu = sy-langu (inicializado en MAIN)

  " Fallback a cualquier idioma si no hay en gv_langu
  IF sy-subrc <> 0.
    SELECT SINGLE vtext
      INTO @cv_vkorg_tx
      FROM tvkot
     WHERE vkorg = @iv_vkorg.
  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
* Devuelve nombre de Centro (T001W)
*---------------------------------------------------------------------*
FORM get_werks_name USING    iv_werks    TYPE werks_d
                    CHANGING cv_werks_tx TYPE t001w-name1.

  CLEAR cv_werks_tx.
  SELECT SINGLE name1
    INTO @cv_werks_tx
    FROM t001w
   WHERE werks = @iv_werks.

ENDFORM.

*---------------------------------------------------------------------*
* FORM BUILD_ALV – Versión compatible
*---------------------------------------------------------------------*
FORM build_alv .

  DATA: lo_alv    TYPE REF TO cl_salv_table,
        lo_fun    TYPE REF TO cl_salv_functions_list,
        lo_disp   TYPE REF TO cl_salv_display_settings,
        lo_cols   TYPE REF TO cl_salv_columns_table,
        lo_col    TYPE REF TO cl_salv_column,
        lo_aggr   TYPE REF TO cl_salv_aggregations,
        lo_layout TYPE REF TO cl_salv_layout,
        ls_key    TYPE salv_s_layout_key.
  "---------------------------------------------------------------*
  " Banner superior (Top-of-List) con datos de ejecución
  "---------------------------------------------------------------*
  DATA: lo_top      TYPE REF TO cl_salv_form_layout_grid,
        lv_vkorg_tx TYPE tvkot-vtext,
        lv_werks_tx TYPE t001w-name1,
        lv_date     TYPE char20,
        lv_time     TYPE char20.


  IF gt_out_base IS INITIAL.
    MESSAGE 'No records found for the selected criteria.' TYPE 'I'.
    RETURN.
  ENDIF.

  " Ordenar por MATNR antes del ALV (tu versión de SALV no soporta orden programático)
  SORT gt_out_base BY matnr.

  TRY.
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table = lo_alv
        CHANGING
          t_table      = gt_out_base ).
    CATCH cx_salv_msg INTO DATA(lx).
      MESSAGE lx->get_text( ) TYPE 'E'.
  ENDTRY.

  lo_fun = lo_alv->get_functions( ).
  lo_fun->set_all( abap_true ). " activar todas las funciones estándar del ALV

  " Layout: clave + permisos de guardado
  lo_layout = lo_alv->get_layout( ).
  ls_key-report = sy-repid.                     " <- clave de layout
  lo_layout->set_key( ls_key ).
  lo_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).
  lo_layout->set_default( abap_true ).

  lo_disp = lo_alv->get_display_settings( ).
  lo_disp->set_list_header( |Inventory Situation Report  - { sy-datum }| ).
  lo_disp->set_striped_pattern( abap_true ).

  lo_cols = lo_alv->get_columns( ).
  lo_cols->set_optimize( abap_true ).

  " Banner superior (Top-of-List)
  " Obtener descripciones
  PERFORM get_vkorg_name USING p_vkorg CHANGING lv_vkorg_tx.
  PERFORM get_werks_name USING p_werks CHANGING lv_werks_tx.

  " Formatear fecha/hora a preferencia del usuario
  lv_date = |{ sy-datum DATE = USER }|.
  lv_time = |{ sy-uzeit TIME = USER }|.

  CREATE OBJECT lo_top.

  " ---- Fila 1: Título centrado (puedes ajustar el texto/text symbol)
  lo_top->add_row( )->create_text(
    text = |INVENTORY SITUATION|
  ).

  " ---- Fila 2: Org. Ventas (número y nombre)
  DATA(lo_row) = lo_top->add_row( ).
  lo_row->create_label( text = 'Org. Ventas:' ).
  lo_row->create_text( text = |{ p_vkorg } - { lv_vkorg_tx }| ).

  " ---- Fila 3: Centro (número y nombre)
  lo_row = lo_top->add_row( ).
  lo_row->create_label( text = 'Centro:' ).
  lo_row->create_text( text = |{ p_werks } - { lv_werks_tx }| ).

  " ---- Fila 4: Fecha / Hora / Usuario
  lo_row = lo_top->add_row( ).
  lo_row->create_label( text = 'Fecha:'   ).
  lo_row->create_text( text = lv_date    ).
  lo_row->create_label( text = 'Hora:'    ).
  lo_row->create_text( text = lv_time    ).

  lo_row = lo_top->add_row( ).
  lo_row->create_label( text = 'Usuario:' ).
  lo_row->create_text( text = sy-uname   ).

  " Asignar al ALV
  lo_alv->set_top_of_list( lo_top ).

  "--- Etiquetas de columnas ---
  lo_col = lo_cols->get_column( 'MATNR'      ).
  lo_col->set_long_text( 'Reference' ).
  lo_col->set_medium_text( 'Ref.' ).
  lo_col->set_short_text( 'Ref.' ).

  lo_col = lo_cols->get_column( 'EAN11'      ).
  lo_col->set_long_text( 'UPC Code' ).
  lo_col->set_medium_text( 'UPC' ).
  lo_col->set_short_text( 'UPC' ).

  lo_col = lo_cols->get_column( 'MAKTX'      ).
  lo_col->set_long_text( 'Description' ).
  lo_col->set_medium_text( 'Desc.' ).
  lo_col->set_short_text( 'Desc.' ).

  lo_col = lo_cols->get_column( 'ALT_UOM_QTY').
  lo_col->set_long_text( 'Units in Packs' ).
  lo_col->set_medium_text( 'Pk' ).
  lo_col->set_short_text( 'Pk' ).

  lo_col = lo_cols->get_column( 'BASE_UNIT'  ).
  lo_col->set_long_text( 'Base Unit' ).
  lo_col->set_medium_text( 'BU' ).
  lo_col->set_short_text( 'BU' ).

  lo_col = lo_cols->get_column( 'LGORT'  ).
  lo_col->set_long_text( 'Storage Location' ).
  lo_col->set_medium_text( 'Stor.Loc' ).
  lo_col->set_short_text( 'St.L' ).

  lo_col = lo_cols->get_column( 'QTY_ON_HAND').
  lo_col->set_long_text( 'Quantity On Hand' ).
  lo_col->set_medium_text( 'Qty On H' ).
  lo_col->set_short_text( 'On Hand' ).

  lo_col = lo_cols->get_column( 'PO_QTY'     ).
  lo_col->set_long_text( 'Purc. Orders Qty' ).
  lo_col->set_medium_text( 'PO Qty.' ).
  lo_col->set_short_text( 'PO Qty.' ).

  lo_col = lo_cols->get_column( 'STO_QTY'    ).
  lo_col->set_long_text( 'Stock Transf. Orders Qty' ).
  lo_col->set_medium_text( 'STO Qty' ).
  lo_col->set_short_text( 'STO Qty' ).

  lo_col = lo_cols->get_column( 'SO_QTY'     ).
  lo_col->set_long_text( 'Sales Orders Qty' ).
  lo_col->set_medium_text( 'SO Qty.' ).
  lo_col->set_short_text( 'SO Qty.' ).

  lo_col = lo_cols->get_column( 'DELIV_QTY'  ).
  lo_col->set_long_text( 'Conduces' ).
  lo_col->set_medium_text( 'Conduces' ).
  lo_col->set_short_text( 'Cond.' ).

  lo_col = lo_cols->get_column( 'QTY_AVAIL'  ).
  lo_col->set_long_text( 'Qty Available' ).
  lo_col->set_medium_text( 'Qty Aval.' ).
  lo_col->set_short_text( 'Qty Aval.' ).

  lo_col = lo_cols->get_column( 'COST'       ).
  lo_col->set_long_text( 'Cost' ).
  lo_col->set_medium_text( 'Cost' ).
  lo_col->set_short_text( 'Cost' ).

  lo_col = lo_cols->get_column( 'TOTAL_COST' ).
  lo_col->set_long_text( 'Total Cost' ).
  lo_col->set_medium_text( 'T. Cost' ).
  lo_col->set_short_text( 'T. Cost' ).

  lo_col = lo_cols->get_column( 'REG_PRICE'  ).
  lo_col->set_long_text( 'Regular Price' ).
  lo_col->set_medium_text( 'R. Price' ).
  lo_col->set_short_text( 'R.Price' ).

  lo_col = lo_cols->get_column( 'TOTAL_SALES').
  lo_col->set_long_text( 'Total Sales' ).
  lo_col->set_medium_text( 'T.Sales' ).
  lo_col->set_short_text( 'T.Sales' ).

  lo_col = lo_cols->get_column( 'RAUBE').
  lo_col->set_long_text( 'Cond. de Almacenaje' ).
  lo_col->set_medium_text( 'Cond.Alm.' ).
  lo_col->set_short_text( 'CA' ).

  "--- Alinear números a la derecha ---
  DATA lt_numcols TYPE STANDARD TABLE OF salv_de_column.
  lt_numcols = VALUE #( ( 'ALT_UOM_QTY' ) ( 'QTY_ON_HAND' ) ( 'PO_QTY' ) ( 'STO_QTY' )
                        ( 'SO_QTY' ) ( 'DELIV_QTY' ) ( 'QTY_AVAIL' )
                        ( 'COST' ) ( 'TOTAL_COST' ) ( 'REG_PRICE' ) ( 'TOTAL_SALES' ) ( 'RAUBE' ) ).

  LOOP AT lt_numcols INTO DATA(lv_colname).
    TRY.
        lo_col = lo_cols->get_column( lv_colname ).
        lo_col->set_alignment( if_salv_c_alignment=>right ).
      CATCH cx_salv_not_found.
    ENDTRY.
  ENDLOOP.

  "---------------------------------------------------------------*
  " Quitar decimales a las cantidades (mostrar 0 decimales en ALV)
  "---------------------------------------------------------------*
  DATA lt_qtycols TYPE STANDARD TABLE OF salv_de_column.
  lt_qtycols = VALUE #(
      ( 'ALT_UOM_QTY' )
      ( 'QTY_ON_HAND' )
      ( 'PO_QTY' )
      ( 'STO_QTY' )
      ( 'SO_QTY' )
      ( 'DELIV_QTY' )
      ( 'QTY_AVAIL' )
  ).

  LOOP AT lt_qtycols INTO DATA(lv_qty_col).
    TRY.
        lo_col = lo_cols->get_column( lv_qty_col ).
        lo_col->set_decimals( '0' ).
      CATCH cx_salv_not_found.
    ENDTRY.

  ENDLOOP.


  lo_aggr = lo_alv->get_aggregations( ).
  TRY.
      lo_aggr->add_aggregation( 'TOTAL_COST' ).
      lo_aggr->add_aggregation( 'TOTAL_SALES' ).
    CATCH cx_salv_not_found.
      " Si la columna no existe, no interrumpas el display
  ENDTRY.

  " Mostrar ALV
  lo_alv->display( ).

ENDFORM.


