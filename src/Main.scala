/* -------------------------------------------------------- */
/* Funciones principales del juego                          */
/* -------------------------------------------------------- */
import utils._
import malla.Partida


object Main {

  val msg_menú = "1) Realizar movimiento\n" +
                 "2) Guardar\n" + 
                 "0) Salir\n"

  val msg_mov  = "\t0: Arriba\n" +
                 "\t1: Abajo\n" +
                 "\t2: Derecha\n" +
                 "\t3: Izquierda\n"

  /**
   * Función inicial.
   *
   * Se encarga de llamar a todas las funciones necesarias para obtener los argumentos
   * e iniciar el bucle principal.
   */
  def main (args: Array[String]) {

    val partida: Partida = Utils.obtener_args (args)
    val l = Utils.crear_lista (partida.filas * partida.columnas)

    partida.imprimir_matriz (l);

    menú (partida, (0, l))
  }


  /**
   * Bucle principal.
   * Imprime el menú y ejecuta el código según la opción seleccionada.
   *
   * @param partida
   *          Objeto con la información del juego
   *
   * @param estado
   *          Tupla con la puntuación y la matriz de juego (en ese orden)
   */
  def menú (partida: Partida, estado: (Int, List [Any])): Unit = {

    print (msg_menú +
           "--> Introduzca la opción seleccionada: ")

    Utils.pedir_opción (0, 2) match {

      case 0 => println (" ---- FIN ---- "); sys.exit (0)
      case 1 => {

        val pos: (Int, Int) = pedir_pos (partida)
        val l: List [Any] = mover (pos
                                  , partida
                                  , pedir_mov (pos, partida)
                                  , estado
                            )

        partida.imprimir_matriz (comprobar_matriz (partida, l))

      }
      case 2 => guardar (partida, estado); menú (partida, estado)
      case _ => println ("Opción no reconocida")
    }
  }

  /**
   * Comprueba las coincidencias en la matriz y devuelve una nueva matriz de booleanos
   * marcando las posiciones a ser eliminadas.
   *
   * @param lista
   *          Matriz de juego con los elementos a ser comprobados
   *
   *
   * @return
   *          Una lista de booleanos con las posiciones consecutivas (las que deben ser
   *        eliminadas) a "true"
   */
  def comprobar_matriz (partida: Partida, lista: List [Any]): List [Boolean] = {

    val horiz = comprobar_horiz (partida, lista)
    val vert = comprobar_vert (partida, lista)

    combinar (horiz, vert)
  }

  /**
   * Combina las dos listas de booleanos en una sola, donde se marcarán los valores que
   * deben ser eliminados. Si no tienen la misma longitud, se devolverá una lista
   * con el mismo número de elementos que 'horiz' (la primera lista).
   *
   * @param horiz
   *          Lista de booleanos marcando las repeticiones horizontales
   *
   * @param vert
   *          Lista de booleanos marcando las repeticiones verticales
   *
   *
   * @return
   *          Una nueva lista con el resultado de (horiz || vert)
   */
  def combinar (horiz: List [Boolean], vert: List [Boolean]): List [Boolean] = {

    val long = vert.length

    Utils.mapear_indexado (horiz) { (e, i) => if (i >= long) e else (e || vert (i)) }
  }

  /**
   * Comprueba las coincidencias de elementos en las horizontales de la matriz.
   *
   * @param partida
   *          Objeto con la información del juego
   *
   * @param lista
   *          Lista con la matriz de juego
   *
   *
   * @return
   *          Una lista de booleanos con las posiciones detectadas como repetidas
   */
  def comprobar_horiz (partida: Partida, lista: List [Any]): List [Boolean] = {

    val long = lista.length

    Utils.mapear_indexado (lista) {
      (e, i) => {
        (
          /* A = b = c */
          ((i >= 0) && (i < long - 1)
            && Utils.misma_fila (i, i + 1, i + 2, partida)
            && (e == lista (i + 1)) && (e == lista (i + 2))
          )
          ||
          /* a = B = c */
          ((i >= 1) && (i < long)
            && Utils.misma_fila (i - 1, i, i + 1, partida)
            && (e == lista (i - 1)) && (e == lista (i + 1))
          )
          ||
          /* a = b = C */
          ((i >= 2) && (i < long + 1)
            && Utils.misma_fila (i - 2, i - 1, i, partida)
            && (e == lista (i - 2)) && (e == lista (i - 1))
          )
        )
      }
    }

  }

  /**
   * Comprueba las coincidencias de elementos en las horizontales de la matriz.
   *
   * @param partida
   *          Objeto con la información del juego
   *
   * @param lista
   *          Lista con la matriz de juego
   *
   *
   * @return
   *          Una lista de booleanos con las posiciones detectadas como repetidas
   */
  def comprobar_vert (partida: Partida, lista: List [Any]): List [Boolean] = {

    val long = lista.length
    val cols = partida.columnas

    Utils.mapear_indexado (lista) {
      (e, i) => {
        (
          /* A = b = c */
          ((i >= 0) && (i + (cols * 2) < long)
            && Utils.misma_columna (i, i + cols, i + (cols * 2), partida)
            && (e == lista (i + cols)) && (e == lista (i + (cols * 2)))
          )
          ||
          /* a = B = c */
          ((i >= cols) && (i + cols < long)
            && Utils.misma_columna (i - cols, i, i + cols, partida)
            && (e == lista (i - cols)) && (e == lista (i + cols))
          )
          ||
          /* a = b = C */
          ((i >= (2 * cols)) && (i < long + 1)
            && Utils.misma_columna (i - (2 * cols), i - cols, i, partida)
            && (e == lista (i - (2 * cols))) && (e == lista (i - cols))
          )
        )
      }
    }

  }

  /**
   * Controla el proceso para guardar el juego en un archivo
   *
   * @param partida
   *          Objeto con la información del juego
   *
   * @param estado
   *          Tupla con la puntuación y la matriz de juego (en ese orden)
   */
  def guardar (partida: Partida, estado: (Int, List [Any])): Unit = {

    print (" --> Introduzca el nombre del archivo en el que guardar los datos: ")
    val nombre = scala.io.StdIn.readLine ()

    Utils.toFile (new java.io.File (nombre)) {
      p => p.println (partida.toString (estado))
    }

    println ("\nArchivo guardado con éxito \n")
  }


  /**
   * Pide la información necesaria para realizar el movimiento y la devuelve
   *
   * @param partida
   *          Objeto con la información del juego
   *
   * @return
   *          Una tupla con la fila y la columna (en ese orden)
   */
  def pedir_pos (partida: Partida): (Int, Int) = {

    print ("--> Introduzca la pieza a mover:\n" +
            "\t-> Fila (entre 0 y " + (partida.filas - 1) + "): ")

    val fila = Utils.pedir_opción (0, partida.filas)

    print ("\t-> Columna (entre 0 y " + (partida.columnas - 1) + "): ")
    val col = Utils.pedir_opción (0, partida.columnas)

    (fila, col)
  }

  /**
   * Pide el movimiento a realizar
   *
   * @param pieza
   *          Pieza que se desea mover
   *
   * @param partida
   *          Objeto con la información del juego
   *
   * @return
   *          El movimiento a realizar, siendo un valor de los siguientes:
   *            0 - Arriba
   *            1 - Abajo
   *            2 - Derecha
   *            3 - Izquierda
   */
  def pedir_mov (pieza: (Int, Int), partida: Partida): Int = {

    print ("\tSeleccione un movimiento de los siguientes: \n" +
           msg_mov +
           "\t-> Movimiento: ")

    val movimiento: Int = Utils.pedir_opción (0, 3)

    if (mov_posible (pieza, partida, movimiento)) {

      movimiento
    } else {

      Utils.log_error ("Ese movimiento no es posible")
      pedir_mov (pieza, partida)
    }
  }

  /**
   * Comprueba si el movimiento es válido o no, teniendo en cuenta la posición de la
   * casilla.
   *
   * @param pieza
   *          Pieza que se desea mover
   *
   * @param partida
   *          Objeto con la información del juego
   *
   * @param movimiento
   *          Movimiento que se desea realizar, siendo un valor de los siguientes:
   *            0 - Arriba
   *            1 - Abajo
   *            2 - Derecha
   *            3 - Izquierda
   */
  def mov_posible (pieza: (Int, Int), partida: Partida, movimiento: Int): Boolean = {

    val fila = pieza._1
    val col = pieza._2

    movimiento match {
      /* Arriba (no se puede mover si está en la primera fila) */
      case 0 => (fila != 0)
      /* Abajo (no se puede mover si está en la última fila) */
      case 1 => (fila != (partida.filas - 1))
      /* Derecha (no se puede mover si está en la última columna) */
      case 2 => (col != (partida.columnas - 1))
      /* Izquierda (no se puede mover si está en la primera columna) */
      case 3 => (col != 0)
      case _ => false
    }

  }

  /**
   * Crea una nueva lista cambiado el elemento a mover con su vecino (arriba, abajo,
   * derecha o izquierda)
   *
   * @param pieza
   *          Pieza que se desea mover
   *
   * @param partida
   *          Objeto con la información del juego
   *
   * @param estado
   *          Tupla con la puntuación y la matriz de juego (en ese orden)
   *
   * @param movimiento
   *          Movimiento que se desea realizar, siendo un valor de los siguientes:
   *            0 - Arriba
   *            1 - Abajo
   *            2 - Derecha
   *            3 - Izquierda
   *
   *
   * @return
   *          Una lista con los elementos cambiados
   */
  def mover (elemento: (Int, Int)
            , partida: Partida
            , movimiento: Int
            , estado: (Int, List [Any])): List [Any] = {

    /* Elige la opción adecuada en función del movimiento */
    movimiento match {
      /* Arriba */
      case 0 => cambiar (estado._2
                        , (elemento._1 * partida.columnas) + elemento._2
                        , ((elemento._1 - 1) * partida.columnas) + elemento._2
                )
      /* Abajo */
      case 1 => cambiar (estado._2
                        , (elemento._1 * partida.columnas) + elemento._2
                        , ((elemento._1 + 1) * partida.columnas) + elemento._2
                )
      /* Derecha */
      case 2 => cambiar (estado._2
                        , (elemento._1 * partida.columnas) + elemento._2
                        , (elemento._1 * partida.columnas) + elemento._2 + 1
                )
      /* Izquierda */
      case 3 => cambiar (estado._2
                        , (elemento._1 * partida.columnas) + elemento._2
                        , (elemento._1 * partida.columnas) + elemento._2 - 1
                )
    }
  }

  /**
   * Cambia el primer elemento por el segundo en la lista especificada.
   *
   * @param lista
   *          Lista en la que se van a intercambiar los elementos
   *
   * @param pos_1
   *          Primera posición a intercambiar (fila, columna)
   *
   * @param pos_2
   *          Segunda posición a intercambiar (fila, columna)
   *
   *
   * @return
   *          Lista con los elementos intercambiados.
   */
  def cambiar (lista: List [Any], pos_1: Int, pos_2: Int): List [Any] = {

    val elem_1 = lista (pos_1)
    val elem_2 = lista (pos_2)

    Utils.insertar (elem_2, pos_1, Utils.insertar (elem_1, pos_2, lista))
  }

}
