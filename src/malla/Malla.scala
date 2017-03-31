package malla

/**
 * Clase para representar la malla del juego.
 *
 * @param dim_filas
 *        Nº de filas que debe tener la matriz de juego.
 *
 * @param dim_cols 
 *        Nº de columnas que debe tener la matriz de juego.
 */
class Malla (dim_filas: Int, dim_cols: Int) {

  val filas: Int = dim_filas;
  val columnas: Int = dim_cols;
  val matriz: List [Int] = inicializar_matriz;

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
   *
   * @return
   *          El elemento en la posición fila:columna de la matriz de juego.
   *          -1 si la posición no existe.
   */
  def obtener (fila: Int, col: Int): Int = {

    val idx = ((fila * columnas) + col)

    if (idx > matriz.length) -1 else matriz(idx)
  }

}