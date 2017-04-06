/* -------------------------------------------------------- */
/* Funciones principales del juego                          */
/* -------------------------------------------------------- */
package main

import utils._
import malla.Malla


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

    val malla: Malla = Utils.obtener_args (args)

    println ("Malla: " + malla.matriz (0))
    malla.imprimir_matriz ();

    menú (malla)
  }

  /**
   * Imprime el menú y ejecuta el código según la opción seleccionada.
   *
   * @param malla
   *          Objeto con la información del juego
   */
  def menú (malla: Malla): Unit = {

    print (msg_menú +
           "--> Introduzca la opción seleccionada: ")

    Utils.pedir_opción (0, 2) match {

      case 0 => println (" ---- FIN ---- "); sys.exit (0)
      case 1 => {

        val pos: (Int, Int) = pedir_pos (malla)

        mover (pos, malla, pedir_mov (pos, malla))
      }
      case 2 => guardar (malla)
      case _ => println ("Opción no reconocida")
    }

    menú (malla)
  }

  /**
   * Controla el proceso para guardar el juego en un archivo
   *
   * @param malla
   *          Objeto con la información del juego
   */
  def guardar (malla: Malla): Unit = {

    print (" --> Introduzca el nombre del archivo en el que guardar los datos: ")
    val nombre = scala.io.StdIn.readLine ()

    Utils.toFile (new java.io.File (nombre)) {p => p.println (malla.toString) }

    println ("\nArchivo guardado con éxito \n")
  }


  /**
   * Pide la información necesaria para realizar el movimiento y la devuelve
   *
   * @param malla
   *          Objeto con la información del juego
   *
   * @return
   *          Una tupla con la fila y la columna (en ese orden)
   */
  def pedir_pos (malla: Malla): (Int, Int) = {

    print ("--> Introduzca la pieza a mover:\n" +
            "\t-> Fila (entre 0 y " + malla.filas + "): ")

    val fila = Utils.pedir_opción (0, malla.filas)

    print ("\t-> Columna (entre 0 y " + malla.columnas + "): ")
    val col = Utils.pedir_opción (0, malla.columnas)

    (fila, col)
  }

  /**
   * Pide el movimiento a realizar
   *
   * @param pieza
   *          Pieza que se desea mover
   *
   * @param malla
   *          Objeto con la información del juego
   *
   * @return
   *          El movimiento a realizar, siendo un valor de los siguientes:
   *            0 - Arriba
   *            1 - Abajo
   *            2 - Derecha
   *            3 - Izquierda
   */
  def pedir_mov (pieza: (Int, Int), malla: Malla): Int = {

    print ("\tSeleccione un movimiento de los siguientes: \n" +
           msg_mov +
           "\t-> Movimiento: ")

    val mov: Int = Utils.pedir_opción (0, 3)

    if (mov_posible (pieza, malla, mov)) {

      mov
    } else {

      Utils.log_error ("Ese movimiento no es posible")
      pedir_mov (pieza, malla)
    }
  }

  /**
   * Comprueba si el movimiento es válido o no, teniendo en cuenta la posición de la
   * casilla.
   *
   * @param pieza
   *          Pieza que se desea mover
   *
   * @param malla
   *          Objeto con la información del juego
   *
   * @param movimiento
   *          Movimiento que se desea realizar, siendo un valor de los siguientes:
   *            0 - Arriba
   *            1 - Abajo
   *            2 - Derecha
   *            3 - Izquierda
   */
  def mov_posible (pieza: (Int, Int), malla: Malla, movimiento: Int): Boolean = {

    val fila = pieza._1
    val col = pieza._2

    movimiento match {
      /* Arriba (no se puede mover si está en la primera fila) */
      case 0 => (fila != 0)
      /* Abajo (no se puede mover si está en la última fila) */
      case 1 => (fila != (malla.filas - 1))
      /* Derecha (no se puede mover si está en la última columna) */
      case 2 => (col != (malla.columnas - 1))
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
   * @param malla
   *          Objeto con la información del juego
   *
   * @param movimiento
   *          Movimiento que se desea realizar, siendo un valor de los siguientes:
   *            0 - Arriba
   *            1 - Abajo
   *            2 - Derecha
   *            3 - Izquierda
   */
  def mover (elemento: (Int, Int), malla: Malla, movimiento: Int): Unit = {

    val x = elemento._1
    val y = elemento._2
println ("------------------")
    /* Elige la opción adecuada en función del movimiento */
    movimiento match {
      /* Arriba */
      case 0 => malla.imprimir_matriz ( cambiar (malla, elemento, (x - 1, y))  )
      /* Abajo */
      case 1 => malla.imprimir_matriz ( cambiar (malla, elemento, (x + 1, y))  )
      /* Derecha */
      case 2 => malla.imprimir_matriz ( cambiar (malla, elemento, (x, y + 1))  )
      /* Izquierda */
      case 3 => malla.imprimir_matriz ( cambiar (malla, elemento, (x, y - 1))  )
    }

println ("------------------")
  }

  /**
   * Cambia el primer elemento por el segundo en la lista especificada.
   *
   * @param malla
   *          Objeto con la información del juego
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
  def cambiar (malla: Malla, pos_1: (Int, Int), pos_2: (Int, Int)): List [Any] = {

    val idx_1 = ((pos_1._1 * malla.columnas) + pos_1._2)
    val idx_2 = ((pos_2._1 * malla.columnas) + pos_2._2)

    val elem_1 = malla.matriz (idx_1)
    val elem_2 = malla.matriz (idx_2)

    malla.matriz.zipWithIndex.map {

      case (e, i) => {

        if (i == idx_1) elem_2 else if (i == idx_2) elem_1 else e
      }
    }

  }


}
