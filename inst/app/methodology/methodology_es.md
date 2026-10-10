## Resumen

Las iniciativas digitales compiten por el mismo presupuesto y las mismas personas. Esta metodología decide cuáles merecen ejecutarse, con un **esfuerzo de evaluación proporcional a lo que está en juego**, y verifica después si el valor prometido se entregó realmente. Sus resultados se usan para mejorar las estimaciones futuras.

El ciclo de vida tiene dos vistas y seis pasos:

| Vista | Paso | Propósito | Quién | Cuándo |
|---|---|---|---|---|
| **Evaluación** (*Appraisal*, antes de la ejecución) | 1 · Registro y RICE | Describir la idea y puntuarla cualitativamente | Todos | Siempre |
| | 2 · Valoración 4MC | Cuantificar valor y costo con fórmulas trazables | Superusuario | Por encima del umbral de valoración |
| | 3 · Revisión experta | Evaluación independiente, con sus propias cifras 4MC | Superusuario | Por encima del umbral de revisión |
| | 4 · Decisión | Priorizar o rechazar | Superusuario | Cuando la evaluación está completa |
| **Realización** (*Realisation*, después de la ejecución) | 5 · Ejecución | Entregar la iniciativa | Superusuario | Después de la priorización |
| | 6 · Auditoría de valor | Medir el valor realmente entregado y su adopción | Superusuario | Después del cierre |

### Principios

1. **Proporcionalidad.** Toda iniciativa recibe una puntuación RICE rápida. La valoración monetaria solo se exige por encima del *umbral de valoración* y la revisión experta solo por encima del *umbral de revisión*. Las ideas pequeñas avanzan rápido; las grandes apuestas reciben escrutinio.
2. **Un mismo lenguaje en todo el ciclo.** Las mismas cinco métricas (**4MC**: Producción, Reservas, Monetario, Tiempo ahorrado y Costo) se usan en la estimación, la revisión experta y la auditoría, así que las tres cifras son directamente comparables.
3. **Valor y costo se mantienen separados.** El costo nunca se resta ni se suma al valor. Se reportan ambos, junto con la relación valor/costo.
4. **Trazabilidad.** Cada cifra se guarda con su fórmula, parámetros, comentario, autor y fecha. Cualquier valoración se puede releer y recalcular.
5. **Ciclo de aprendizaje.** Dos regresiones calibradas anticipan el valor a partir de los datos RICE, y el valor realizado a partir de la evaluación ex-ante. Se recalibran a medida que se acumulan auditorías y se publican como versiones.
6. **Registro abierto, evaluación controlada.** Cualquiera puede proponer una iniciativa. Las evaluaciones y decisiones las toman los superusuarios.
7. **Proceso transparente.** Todos los pasos son visibles para todos, y las evaluaciones faltantes se resaltan. La actividad de los usuarios se registra para analizar y mejorar el propio proceso.

## 1 · Registro y RICE

La iniciativa se registra con nombre, descripción breve, responsable, unidad de negocio, costo estimado y fechas previstas. **Los datos RICE se registran en el mismo formulario.**

**RICE** es una puntuación cualitativa para comparar y ordenar iniciativas:

`Puntuación RICE = log10(usuarios alcanzados) × Impacto × Confianza ÷ Esfuerzo`

* **Alcance** (*Reach*) es el número de usuarios impactados. Se usa su logaritmo en base 10, así que pasar de 10 a 100 usuarios cuenta lo mismo que pasar de 1.000 a 10.000. Esto evita que las audiencias muy grandes dominen el ranking.
* **Impacto** y **Esfuerzo** se expresan en tallas (XS–XL). La **Confianza** va de *Moonshot* (apuesta) a *Certain* (seguro).

{{rice_weights_table}}

*Alcance × Impacto × Confianza* es el **valor** cualitativo. Al dividirlo por el peso del esfuerzo se obtiene la puntuación. El gráfico del Portafolio muestra el valor frente al esfuerzo, de modo que una posición alta y a la izquierda es una victoria rápida.

**Bloqueo.** Una vez guardados, el registro y su RICE quedan bloqueados para el contribuidor que los creó. Solo un superusuario puede editarlos. Así la evaluación inicial se mantiene honesta y comparable.

**Valor anticipado.** Mientras se ingresan los datos RICE, el formulario muestra el valor ex-ante esperado para iniciativas similares, con un rango. Proviene del modelo calibrado publicado (ver *Modelos de valor calibrados*).

## Umbrales de evaluación

Dos umbrales deciden cuánta evaluación necesita una iniciativa. Son parámetros de configuración.

| Umbral | Se activa cuando | Exige |
|---|---|---|
| **Umbral de valoración** | Esfuerzo ≥ **{{gate2_effort_min}}** o puntuación RICE ≥ **{{gate2_score_min}}** | Valoración 4MC |
| **Umbral de revisión** | Valor ex-ante ≥ **{{gate3_value_min_mm_usd}} mm USD** o costo ≥ **{{gate3_cost_min_mm_usd}} mm USD** | Revisión experta |

* El **valor ex-ante** es el valor de la revisión experta cuando esta aprobó la iniciativa; si no, la estimación 4MC.
* El **costo planificado** es el costo de la revisión aprobada; si no, la estimación de costo 4MC; si no, el costo ingresado en el registro.

El estado siempre muestra el siguiente paso pendiente: *Registered → 4MC valuation → Expert review → Ready*. Cuando falta un paso requerido, se resalta en ámbar en la pestaña Initiative y se lista en el Portafolio.

## 2 · Valoración 4MC

La valoración 4MC cuantifica la iniciativa con cinco métricas:

| Métrica | Unidad | Conversión a mm USD | Horizonte |
|---|---|---|---|
| **P** · Producción | BOPD incrementales promedio del año | BOPD × {{days_per_year}} días × net back {{netback_usd_bbl}} USD/bbl | Anual |
| **R** · Reservas | MMbbl, por categoría | MMbbl × valor por barril de la categoría (tabla abajo) | Única vez |
| **M** · Monetario | mm USD por año | Tal como se ingresa | Anual |
| **T** · Tiempo ahorrado | miles de horas por año | khoras × 1.000 × {{time_value_usd_h}} USD/h | Anual |
| **C** · Costo | mm USD | Tal como se ingresa; **separado del valor** | Período de implementación |

{{reserve_table}}

El **tiempo ahorrado** se valora al salario promedio ({{avg_salary_usd_year}} USD/año sobre {{work_hours_year}} h) multiplicado por un coeficiente de productividad de **{{time_productivity_coef}}**. El coeficiente refleja que el tiempo liberado de tareas rutinarias se dedica a tareas más productivas.

**Líneas de cálculo.** Una valoración se compone de líneas. Cada línea usa un método de una métrica, por ejemplo:

* Producción: estimación directa; trabajos × tasa por trabajo × mejora de la tasa de éxito; mejora de disponibilidad; mitigación de la declinación.
* Reservas: estimación directa; mejora del factor de recobro sobre el petróleo original en sitio.
* Monetario: estimación directa; eventos × ahorro por evento; costo evitado × reducción de su probabilidad.
* Tiempo ahorrado: estimación directa; usuarios × horas por semana × semanas; tareas × minutos ahorrados.
* Costo: capex + opex del período; esfuerzo en personas-mes × tarifa + licencias.

Cada línea guarda sus parámetros, la fórmula legible y un comentario. Ejemplo: *10 reparaciones (workovers) al año, cada una de 30 BOPD, con la tasa de éxito aumentada del 60% al 70%*:

`10 trabajos × 30 BOPD × (70% − 60%) = {{example_workover_bopd}} BOPD → {{example_workover_mm}} mm USD`

El **valor 4MC** es P + R + M + T (métricas anuales más reservas de única vez). El costo se muestra al lado, con la relación valor/costo. Una valoración con solo líneas de costo no completa el paso: se necesita al menos una métrica de valor.

## 3 · Revisión experta

Por encima del umbral de revisión, un experto (superusuario, normalmente de planificación) evalúa la iniciativa de forma independiente y registra:

* una **decisión**:
  * **Approve** (aprobar): las cifras de la revisión pasan a ser el valor y el costo ex-ante;
  * **Rework** (rehacer): la iniciativa queda pendiente hasta una nueva revisión;
  * **Reject** (rechazar): la iniciativa se rechaza;
* sus **propias cifras 4MC** (P, R, M, T y C). Vienen precargadas con la estimación 4MC y se pueden ajustar, por ejemplo con un descuento por riesgo o una contingencia de costo;
* notas: supuestos, VPN, riesgos, condiciones.

El formulario muestra el valor resultante, la relación valor/costo y el **valor realizado anticipado**. Es decir, lo que iniciativas similares entregaron realmente respecto de su valor ex-ante, según el modelo publicado.

## 4 · Decisión

Cuando la evaluación está completa (estado *Ready*), un superusuario **prioriza** o **rechaza** la iniciativa, con un comentario opcional. Una iniciativa priorizada puede volver a *Ready* o rechazarse antes de iniciar la ejecución.

La pestaña **Portfolio** apoya la decisión:

* el gráfico valor-esfuerzo: Alcance × Impacto × Confianza, valor ex-ante, o valor/estimación, frente al esfuerzo. Los umbrales se dibujan con líneas discontinuas; el de la puntuación aparece como una escalera;
* los indicadores de cartera, valor comprometido, valor/costo y realización;
* el registro con el ranking RICE, valores, costos y evaluaciones faltantes.

## 5 · Ejecución

Una iniciativa priorizada se **inicia** y luego se **cierra** desde la barra de decisión. El cierre de la ejecución habilita la auditoría de valor.

## 6 · Auditoría de valor

Después del cierre, idealmente cuando la solución lleva en uso el tiempo suficiente para medir su efecto (normalmente 6–12 meses), un superusuario registra:

* las **cifras 4MC reales** (P, R, M, T) y el **costo real** (C);
* la **adopción**: la proporción de los usuarios previstos que usan realmente la solución;
* una conclusión y las lecciones aprendidas.

La auditoría compara estimación, revisión experta y valor real, métrica por métrica. Reporta la **tasa de realización** (valor auditado ÷ valor ex-ante) y el sobrecosto (costo real ÷ costo planificado). Una baja adopción suele explicar una baja tasa de realización.

## Modelos de valor calibrados

Dos regresiones log-lineales conectan las etapas del ciclo:

| Modelo | Predice | A partir de |
|---|---|---|
| RICE → valor ex-ante | Valor ex-ante | log10(usuarios), log Impacto, log Confianza, log Esfuerzo |
| Ex-ante → valor realizado | Valor auditado | log valor ex-ante, log Confianza, log Esfuerzo |

* **Por qué logaritmos.** RICE es multiplicativo, así que un modelo log-lineal da *elasticidades*. Por ejemplo, duplicar el impacto multiplica el valor por 2 elevado al coeficiente del impacto.
* **Rango.** La estimación es una mediana con un rango {{p_low}}–{{p_high}}: el intervalo de predicción al {{interval_pct}}% de la regresión, devuelto a mm USD. Un rango amplio significa que, por ahora, los datos RICE explican poco del valor.
* **Pocos datos.** Con menos de **{{value_model_min_n}}** observaciones se usa un modelo más simple, de un solo predictor. Con menos de 4 no se da estimación.
* **Calibración.** En *Admin → Value models* un superusuario ajusta un candidato con los datos actuales. Lo compara con la versión publicada en R², ancho del rango, cobertura del intervalo y coeficientes, y lo publica con un comentario. Todas las versiones se conservan y se pueden reactivar. Las estimaciones solo cambian cuando se publica una nueva versión.

Los modelos son una herramienta de aprendizaje, no un sustituto de la valoración: muestran qué tan buenas fueron las estimaciones pasadas y cuánto confiar en una nueva.

## Roles y responsabilidades

| Acción | Contribuidor | Superusuario |
|---|:-:|:-:|
| Registrar una iniciativa con su RICE | ✓ | ✓ |
| Editar un registro o su RICE después de guardarlo | | ✓ |
| Valoración 4MC, revisión experta, decisiones, auditoría de valor | | ✓ |
| Calibrar y publicar modelos de valor; administración | | ✓ |

Los superusuarios se identifican por su grupo o nombre de usuario en Posit Connect.

## Indicadores y mejora continua

* **Valor en cartera, comprometido y realizado** (mm USD) y **valor/costo** de la cartera comprometida.
* **Tasa de realización**: valor auditado ÷ valor ex-ante. Mide la precisión de las estimaciones.
* **Adopción**: adopción promedio de las iniciativas auditadas.
* **Cumplimiento de umbrales**: proporción de valoraciones 4MC y revisiones expertas requeridas que efectivamente se hicieron.
* **Tiempo de decisión**: mediana de días desde el registro hasta la decisión de priorizar o rechazar.
* **Alertas abiertas**: acciones pendientes del proceso.

La actividad de los usuarios se registra y se agrupa en los pasos del ciclo de vida (*Admin → Process log*). El registro de eventos resultante se puede analizar con herramientas de minería de procesos para encontrar cuellos de botella y retrabajos.

## Glosario

* **BOPD**: barriles de petróleo por día.
* **MMbbl**: millones de barriles.
* **1P / 2P / 3P**: reservas probadas; probadas + probables; probadas + probables + posibles.
* **Recursos contingentes**: volúmenes descubiertos aún no comerciales.
* **Net back**: valor neto por barril después de costos, usado para monetizar la producción.
* **Valor ex-ante**: valor esperado antes de la ejecución (revisión experta si fue aprobada; si no, la estimación 4MC).
* **Valor realizado**: valor medido por la auditoría.
* **Tasa de realización**: valor realizado ÷ valor ex-ante.
* **Adopción**: proporción de los usuarios previstos que usan activamente la solución.
* **Umbral**: límite que hace obligatorio un paso de evaluación adicional.
