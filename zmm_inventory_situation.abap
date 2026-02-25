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

  DATA: lv_idx     TYPE sy-tabix,
        lv_um_bapi TYPE meins,
        lv_knumh   TYPE knumh,
        lv_kbetr   TYPE kbetr,
        ls_konp    TYPE konp.

  " Si prefieres trabajar con una tabla separada:
  " CLEAR gt_out. gt_out = gt_out_base.

  LOOP AT gt_out_base ASSIGNING FIELD-SYMBOL(<ls_out>).
    lv_idx = sy-tabix.

*    CLEAR: gs_mrp_detail, gt_mrp_items, gt_mrp_ind_lines, gt_mrp_total_lines, gt_return[].

    "-----------------------------------------------------------------*
    " 1) Llamar la BAPI por cada Material–Centro
    "-----------------------------------------------------------------*
*    CALL FUNCTION 'BAPI_MATERIAL_STOCK_REQ_LIST'
*      EXPORTING
*        material_long    = <ls_out>-matnr
*        plant            = p_werks
*        mrp_area         = gv_mrp_area
*        plan_scenario    = '000'
*       get_item_details = 'X'
*       ignore_buffer    = ' '         " si quieres forzar datos "frescos", poner 'X'
*      IMPORTING
*        mrp_stock_detail = gs_mrp_detail
*        return           = gs_return.
*      TABLES
*        mrp_items             = gt_mrp_items
*        mrp_ind_lines         = gt_mrp_ind_lines
*        mrp_total_lines       = gt_mrp_total_lines


    " Manejo básico de errores BAPI
*    READ TABLE gt_return WITH KEY type = 'E' TRANSPORTING NO FIELDS.
*    IF sy-subrc = 0.
*      " Si hay error, dejar buckets en 0 y continuar con el siguiente
*      <ls_out>-po_qty    = 0.
*      <ls_out>-sto_qty   = 0.
*      <ls_out>-so_qty    = 0.
*      <ls_out>-deliv_qty = 0.
*      <ls_out>-qty_avail = <ls_out>-qty_on_hand. " sin restar deliveries
*      CONTINUE.
*    ENDIF.

    "-----------------------------------------------------------------*
    " 3) Mapeo directo: MRP_STOCK_DETAIL → Buckets
    "-----------------------------------------------------------------*
    DATA(lv_po)   = CONV menge_d( 0 ).
    DATA(lv_sto)  = CONV menge_d( 0 ).
    DATA(lv_so)   = CONV menge_d( 0 ).
    DATA(lv_dlv)  = CONV menge_d( 0 ).


    " Calculo de PO pendientes por recibir (excl. STO si tu sistema los separa)

    DATA: lv_total_open TYPE mard-labst,
          lv_open_po    TYPE eket-menge,
          lv_open_meins TYPE mard-labst.

    CLEAR: lv_open_po, lv_open_meins, lv_total_open, lv_po.

    SELECT
          ekko~ebeln,
          ekpo~ebelp,
          ekpo~meins,
          eket~menge,
          eket~wemng,
          eket~glmng
      FROM ekko
      INNER JOIN ekpo
         ON ekpo~ebeln = ekko~ebeln
      INNER JOIN eket
         ON eket~ebeln = ekpo~ebeln
        AND eket~ebelp = ekpo~ebelp
      INTO TABLE @DATA(lt_ekpo_rows)
      WHERE ekko~bstyp = 'F'                      " Pedido de compra
        AND ekko~bsart IN ( 'ZIMP', 'ZMER' )      " Solo tipos requeridos
        AND ekko~loekz = @space                   " (opcional) cabecera no borrada
        AND ekpo~loekz = @space                   " posición no borrada
        AND ekpo~elikz = @space                   " sin entrega completa
        AND ekpo~matnr = @<ls_out>-matnr
        AND ekpo~werks = @p_werks                 " destino real por línea
        AND ekpo~lgort = @<ls_out>-lgort
        AND eket~menge > eket~wemng.              " con pendiente (>0)

    IF sy-subrc <> 0.
      lv_po = 0.
    ELSE.
      "-------------------------------
      " Sumar can PO pendiente en MEINS
      "-------------------------------
      LOOP AT lt_ekpo_rows ASSIGNING FIELD-SYMBOL(<r>).
        lv_open_po = <r>-menge - <r>-wemng.
        IF lv_open_po <= 0.
          CONTINUE.
        ENDIF.
        " Convertir de UM de la PO -> MEINS (unidad base del material)
        IF <r>-meins IS NOT INITIAL AND <r>-meins <> <ls_out>-base_unit.
          CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
            EXPORTING
              i_matnr              = <ls_out>-matnr
              i_in_me              = <r>-meins
              i_out_me             = <ls_out>-base_unit
              i_menge              = lv_open_po
            IMPORTING
              e_menge              = lv_open_meins
            EXCEPTIONS
              error_in_application = 1
              error                = 2
              OTHERS               = 3.
          IF sy-subrc <> 0.
            " Fallback conservador: sumar sin conversión (equivalencia 1:1)
            lv_open_meins = lv_open_po.
          ENDIF.
        ELSE.
          lv_open_meins = lv_open_po.
        ENDIF.

        lv_total_open += lv_open_meins.

      ENDLOOP.

      lv_po = lv_total_open.

    ENDIF.

*    " PO pendientes por recibir (excl. STO si tu sistema los separa)
*    lv_po  = gs_mrp_detail-pur_orders.

*    " Entregas abiertas (sin PGI)
*    lv_dlv = gs_mrp_detail-delivery.
*    " Ahora hay que buscar las entregas por almacen
    SELECT SUM( lgmng )
      FROM lips
      INTO @lv_dlv
        WHERE matnr = @<ls_out>-matnr
      AND lgort = @<ls_out>-lgort
      AND wbsta = 'A'.
    IF sy-subrc <> 0 OR lv_dlv IS INITIAL.
      lv_dlv = 0.
    ENDIF.

    " Verificamos que el Almacen sea el del Picking

    CLEAR: lt_ekpo_rows, lv_open_po, lv_open_meins, lv_total_open, lv_so.

    SELECT SINGLE lgort
      INTO @DATA(lv_matlgort)
      FROM tvkol
      WHERE vstel = @p_werks
        AND werks = @p_werks
        AND raube = @<ls_out>-raube.

    IF lv_matlgort = <ls_out>-lgort AND sy-subrc = 0.

*    " STO (traslados). En tu landscape puede estar en uno de estos:
*    lv_sto = gs_mrp_detail-stk_trnf_rel.

      SELECT
            ekko~ebeln,
            ekpo~ebelp,
            ekpo~meins,
            eket~menge,
            eket~wemng,
            eket~glmng
        FROM ekko
        INNER JOIN ekpo
           ON ekpo~ebeln = ekko~ebeln
        INNER JOIN eket
           ON eket~ebeln = ekpo~ebeln
          AND eket~ebelp = ekpo~ebelp
        INTO TABLE @lt_ekpo_rows
        WHERE ekko~bstyp = 'F'                 " Pedido de compra
          AND ekko~bsakz = 'T'                 " Traslado
          AND ekko~reswk = @p_werks             " Suministrador = CD01
          AND ekpo~loekz = @space              " Posición no borrada
          AND ekpo~matnr = @<ls_out>-matnr
          AND eket~menge > eket~glmng.         " Pendiente de entrega

      IF lt_ekpo_rows IS INITIAL.

        lv_sto = 0.

      ELSE.

        "---- Sumar pendiente en MEINS (MARA-MEINS)
        LOOP AT lt_ekpo_rows ASSIGNING FIELD-SYMBOL(<r2>).
          " Open por línea (protección contra negativos)
          lv_open_po = <r2>-menge - <r2>-glmng.
          IF lv_open_po <= 0.
            CONTINUE.
          ENDIF.

          " Convertir de UM de la PO -> MEINS
          IF <r2>-meins IS NOT INITIAL AND <r2>-meins <> <ls_out>-base_unit.
            CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
              EXPORTING
                i_matnr              = <ls_out>-matnr
                i_in_me              = <r2>-meins
                i_out_me             = <ls_out>-base_unit
                i_menge              = lv_open_po
              IMPORTING
                e_menge              = lv_open_meins
              EXCEPTIONS
                error_in_application = 1
                error                = 2
                OTHERS               = 3.
            IF sy-subrc <> 0.
              " Fallback: sumar sin conversión si falla
              lv_open_meins = lv_open_po.
            ENDIF.
          ELSE.
            lv_open_meins = lv_open_po.
          ENDIF.

          lv_total_open += lv_open_meins.
        ENDLOOP.

        lv_sto = lv_total_open.

      ENDIF.

      "-----------------------------------------------------------------*
      " 4) SO_QTY desde VBAP/VBEP: SUM( VBEP-ORDQTY_BU ) en UM base
      "      Ítems del material y centro con estado de entrega A/B
      "-----------------------------------------------------------------*
      SELECT SUM( v~ordqty_bu )
        INTO @lv_so
        FROM vbap AS a
        INNER JOIN vbep AS v
                ON v~vbeln = a~vbeln
               AND v~posnr = a~posnr
       WHERE a~matnr = @<ls_out>-matnr      " material de la fila
         AND a~werks = @p_werks             " centro del reporte
         AND a~lfsta IN ( 'A', 'B' ).       " no entregado / parcialmente entregado

      IF sy-subrc <> 0 OR lv_so IS INITIAL.
        lv_so = 0.
      ENDIF.

    ELSE.

      lv_sto = 0.
      lv_so = 0.

    ENDIF.



    "-----------------------------------------------------------------*
    " 5) Asignar buckets a la fila y calcular QTY_AVAIL
    "-----------------------------------------------------------------*
    <ls_out>-po_qty    = lv_po.
    <ls_out>-sto_qty   = lv_sto.
    <ls_out>-so_qty    = lv_so.
    <ls_out>-deliv_qty = lv_dlv.

    <ls_out>-qty_avail = <ls_out>-qty_on_hand - <ls_out>-deliv_qty.
*    IF <ls_out>-qty_avail < 0.
*      <ls_out>-qty_avail = 0. " por seguridad
*    ENDIF.

    CLEAR: lv_knumh, ls_konp.
    <ls_out>-reg_price = 0.

    "---------------------------------------------------------------*
    " 1) Buscar en A073 el registro de condición VKP0 válido
    "---------------------------------------------------------------*
    SELECT SINGLE knumh
      INTO @lv_knumh
      FROM a073
     WHERE kappl =  'V'
       AND kschl =  'VKP0'
       AND vkorg =  @p_vkorg
       AND vtweg =  @p_vtweg
       AND matnr =  @<ls_out>-matnr
       AND vrkme =  @<ls_out>-base_unit
       AND datab <= @p_prdat
       AND datbi >= @p_prdat.

    IF sy-subrc <> 0.
      " Sin registro en A073 → precio = 0
      <ls_out>-reg_price = 0.
    ELSE.

      "---------------------------------------------------------------*
      " 2) Ahora con el KNUMH, buscar el KBETR en KONP
      "---------------------------------------------------------------*
      SELECT SINGLE kbetr
        FROM konp
        INTO @lv_kbetr
       WHERE knumh = @lv_knumh
         AND kappl = 'V'
         AND kschl = 'VKP0'
         AND loevm_ko = ''.

      IF sy-subrc <> 0.
        <ls_out>-reg_price = 0.
      ELSE.
        <ls_out>-reg_price = lv_kbetr.
      ENDIF.

    ENDIF.

*  Paso 4: Cálculo de TOTAL_COST y TOTAL_SALES
    <ls_out>-total_cost = <ls_out>-qty_avail * <ls_out>-cost.
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


