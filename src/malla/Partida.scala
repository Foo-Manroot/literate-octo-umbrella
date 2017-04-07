/* -------------------------------------------------------- */
/* Estructura para almacenar los datos de la malla de juego */
/* -------------------------------------------------------- */
package malla


/**
 * Clase para representar la malla del juego.
 *
 * @param dim_filas
 *        Nº de filas que debe tener la matriz de juego.
 *
 * @param dim_cols 
 *        Nº de columnas que debe tener la matriz de juego.
 *
 * @param niv
 *        Nivel del juego
 */
class Partida (dim_filas: Int, dim_cols: Int, niv: Int) {

  val filas: Int = dim_filas
  val columnas: Int = dim_cols
  val nivel: Int = niv

  /**
   * Devuelve una cadena con la información de la partida.
   */
  override def toString: String = {

    "Filas: " + filas + "\n"        +
    "Columnas: " + columnas + "\n"  +
    "Nivel: " + nivel + "\n"
  }

  /**
   *  Imprime el contenido de la matriz, ya formateado. Para imprimir el contenido de
   *  la matriz, sólo hace falta usar esta función tal que malla.imprimir_matriz()
   *
   *  @param lista
   *          Lista a ser impresa, con el número de de columnas especificado en la
   *        declaración del objeto. Puede omitirse (por defecto, es 'matriz').
   *
   *  @param contador
   *          Contador para controlar la columna que se está imprimiendo. Puede omitirse
   *        (por defecto, es 1).
   */
  def imprimir_matriz[T] (lista: List[T] = matriz, contador: Int = 1): Unit = {

    lista.zipWithIndex.map {
      /* i = índice; e = elemento */
      case (e, i) =>
        if (i % columnas == (columnas - 1)) print (e + "\n") else print (e + " ")
    }
  }

}
