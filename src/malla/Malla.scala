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
class Malla (dim_filas: Int, dim_cols: Int, niv: Int) {

  val filas: Int = dim_filas
  val columnas: Int = dim_cols
  val matriz: List [Int] = inicializar_matriz
  val nivel: Int = niv

  /*
   *  Inicializa la lista con (filas * columnas) elementos aleatorios.
   *
   *  @return
   *          Una lista con (filas * columnas) enteros aleatorios.
   */
  private def inicializar_matriz: List [Int] = {

    List.fill (filas * columnas)(util.Random.nextInt (10))
  }

  /*
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


  /*
   * Obtiene el elemento en la posición fila:columna.
   *
   * @param fila
   *          Posición Y del elemento a obtener.
   *
   * @param col
   *          Posición X del elemento a obtener.
   *
   * @param lista
   *          Lista de la que se quiere obtener el elemento. Si se deja vacío, se usa
   *        la matriz de esta malla.
   *
   * @return
   *          El elemento en la posición fila:columna de la matriz de juego.
   *          null (como una instancia de tipo T) si la posición no existe.
   */
  def obtener[T] (fila: Int, col: Int, lista: List [T] = matriz): T = {

    val idx = ((fila * columnas) + col)

    if (idx > lista.length) null.asInstanceOf [T] else lista (idx)
  }


  /**
   * Devuelve una cadena con la información de la malla.
   */
  override def toString: String = {

    "Filas: " + filas + "\n"        +
    "Columnas: " + columnas + "\n"  +
    "Nivel: " + nivel + "\n"        +
    "Matriz de juego: \n"           +
    /* matriz.toString devuelve "List(1, 2, 3, ...)"; así que se sustituyen los
     * elementos innecesarios para devolver "1 2 3 4 5..." */
    matriz.toString.replace (',', ' ')
                   .replace ('L', ' ')
                   .replace ('i', ' ')
                   .replace ('s', ' ')
                   .replace ('t', ' ')
                   .replace ('(', ' ')
                   .replace (')', ' ')
                   .trim
  }

}
