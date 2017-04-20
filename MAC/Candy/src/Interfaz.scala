 import javax.swing._   
 import java.awt.event._
 import java.awt.Color
 import java.awt.GridLayout
 import JFrame._ 
 
 import partida._
 import utils.Utils

object GUI_main {

  def main (args: Array [String]) {

    println("Interfaz")
    
    val partida: Partida = Utils.obtener_args (args)
    
    val l = Utils.crear_lista (partida.filas * partida.columnas, partida.nivel)

    val ui = new Interfaz

    ui.crear_ventana (partida, (0, l))
    
    
  }

}

class Interfaz extends JFrame("Jewels Legend Hero") { 
  
  val azul:ImageIcon = new ImageIcon("./img/azul.png")    
  val rojo:ImageIcon = new ImageIcon("./img/rojo.png") 
  val naranja:ImageIcon = new ImageIcon("./img/naranja.png") 
  val verde:ImageIcon = new ImageIcon("./img/verde.png") 
  val plata:ImageIcon = new ImageIcon("./img/plata.png") 
  val morado:ImageIcon = new ImageIcon("./img/morado.png")
  val gris:ImageIcon = new ImageIcon("./img/gris.png") 
  val amarillo:ImageIcon = new ImageIcon("./img/amarillo.png") 
  val vacio:ImageIcon = new ImageIcon("./img/vacio.png") 

  /* Color al pulsar un botón */
  val pulsado: Color = Color.BLACK

  /* Estado del juego */
  var estado: (Int, List [Any]) = _

/*
 * Comportamiento de las teclas para tareas auxiliares:
 *  Q -> Salir
 *  G -> Guardar
 *  C -> Comprobar huecos  
 *  E -> Estrategia óptima
 */
  
  
   def key_bindings (panel: JPanel, partida: Partida) = {
 
     panel.getInputMap ().clear ()
     panel.getActionMap ().clear ()
     /* Teclas */
     panel.getInputMap ().put (KeyStroke.getKeyStroke ("Q"), "Salir");
     panel.getInputMap ().put (KeyStroke.getKeyStroke ("G"), "Guardar");
     panel.getInputMap ().put (KeyStroke.getKeyStroke ("C"), "Comprobar");
     panel.getInputMap ().put (KeyStroke.getKeyStroke ("E"), "Estrategia");
     /* Acciones */
     panel.getActionMap ().put ("Salir", new AbstractAction () {
 
       def actionPerformed (e: ActionEvent) { sys.exit (0) }
     });
     panel.getActionMap ().put ("Guardar", new AbstractAction () {
 
       def actionPerformed (e: ActionEvent) { 
         Utils.toFile (new java.io.File ("partida.txt")) {
           p => p.println (partida.toString (estado))
         }
       }
     });
     panel.getActionMap ().put ("Comprobar", new AbstractAction () {
 
       def actionPerformed (e: ActionEvent) {
         val cambios = Main.eliminar_coicidencias (partida, estado)      
         crear_ventana (partida, cambios)
       }
     });
     panel.getActionMap ().put ("Estrategia", new AbstractAction () {
 
       def actionPerformed (e: ActionEvent) {
         val cambios = Main.estrategia_optima (partida, estado)
         crear_ventana (partida, cambios)
       }
     });
   }
  
  
  /** 
   *  Crea la ventana que muestra la lista de diamantes 
   */
  def crear_ventana (partida: Partida, estado_ini: (Int, List [Any])): Unit = {
    
    val filas = partida.filas
    val columnas = partida.columnas

    estado = estado_ini
    estado = Main.eliminar_coicidencias(partida,estado)
   
    
    setDefaultLookAndFeelDecorated(true) 

    setDefaultCloseOperation(EXIT_ON_CLOSE) 

    
    
    val score: JLabel = new JLabel()
    score.setText("Puntuacion: "+ estado._1)
    
    
    val panel: JPanel = new JPanel()
    panel.setSize(columnas * 50,filas * 50)
    
    

    Utils.mapear (crear_lista_botones (estado, panel, partida)) { b => panel.add (b) }
    
    key_bindings (panel, partida)
    
    panel.add(score)
    
    this.setContentPane(panel)
   
    setSize(50 + columnas * 50,50 + filas * 50)
    setVisible(true);
    setResizable (false)
  }

  /**
   * Crea una lista con todos los botones
   */
  def crear_lista_botones (estado: (Int, List [Any])
                          , panel: JPanel
                          , partida: Partida): List[JButton] = { 

    val boton = crear_boton(estado._2.head, panel, partida)

    if (estado._2.length == 1) {

      boton::Nil
    } else {

      boton::crear_lista_botones((estado._1,estado._2.tail), panel, partida)
    }

  }

  /**
   * Crea un boton segun el identificador
   */
  def crear_boton (id: Any
                  , panel: JPanel
                  , partida: Partida): JButton = {

    val button = new JButton()
    button.setSize(30, 30)

    button.addActionListener (new ActionListener () {
      def actionPerformed (e: ActionEvent) { controlador (e, panel, button, partida) }
    })

    button.setBackground (color (id))

    id match {
      case 1 => button.setIcon(azul);     button
      case 2 => button.setIcon(rojo);     button
      case 3 => button.setIcon(naranja);  button
      case 4 => button.setIcon(verde);    button
      case 5 => button.setIcon(plata);    button
      case 6 => button.setIcon(morado);   button
      case 7 => button.setIcon(gris);     button
      case 8 => button.setIcon(amarillo); button
      case _ => button.setIcon(vacio);    button
    }
  }


  /**
   * Controlador para el botón
   */
  def controlador (e: ActionEvent
                  , panel: JPanel
                  , b: JButton
                  , partida: Partida) = {

    val componentes = panel.getComponents ().toList
    val idx_puls: Int = buscar_pulsado (componentes)

    if (idx_puls == -1) {

      b.setBackground (pulsado)
    } else {

      val idx = buscar_elem (componentes, b)
      intercambiar (idx, idx_puls, partida)

      /* Actualiza el panel */
      panel.removeAll ()
      panel.repaint ()

      Utils.mapear (crear_lista_botones (estado, panel, partida)) { e => panel.add (e) }
    }

  }

  /**
   * Intercambia los dos elementos en la matriz de juego
   */
  
  def intercambiar (idx_1: Int, idx_2: Int, partida: Partida) = {

    val mov = comprobar_mov (idx_1, idx_2, partida)

    if ((mov != -1)&&(Main.mov_valido(Main.obtener_posicion(idx_2,partida),partida,mov,estado))) {

      val l = Utils.cambiar (estado._2, idx_1, idx_2)

      val huecos: List [Any] = Main.eliminar (l, Main.comprobar_matriz (partida, l))
      val puntos: Int = (estado._1 + Main.contar_huecos (huecos))

      val nueva_matriz: List [Any] = Main.tratar_huecos (partida, huecos)

      partida.imprimir_matriz(estado._2)
      println (" ---- ")
      println ("Huecos: ")
      
       
       
      crear_ventana(partida,(estado._1,huecos))
      
      
      partida.imprimir_matriz (huecos)
      
      println (" --Nueva matriz-- ")
      partida.imprimir_matriz (nueva_matriz)
      println (" ---- ")
      
      estado = (puntos, nueva_matriz)
    }
     
    crear_ventana(partida,(estado))
    
  }


  /**
   * Comprueba que el movimiento es válido (es decir, que idx_1 está a 1 casilla de
   * diferencia de idx_2) y devuelve el movimiento correspondiebnte, dentro de los
   * siguientes:
   *            0 - Arriba
   *            1 - Abajo
   *            2 - Derecha
   *            3 - Izquierda
   *
   * Si no es un movimiento válido, devuelve -1
   *
   * Por ejemplo, si se devuelve 2 significa que idx_1 está a la derecha de idx_2
   */
  def comprobar_mov (idx_1: Int, idx_2: Int, partida: Partida): Int = {

    if ((idx_1 - idx_2) == 1) 2
    else if ((idx_1 - idx_2) == -1) 3
    else if ((idx_1 - partida.columnas) == idx_2) 1
    else if ((idx_1 + partida.columnas) == idx_2) 0
    else -1
  }

  /**
   * Busca en la lista un botón pulsado (getBackground () == pulsado)
   */
  def buscar_pulsado (lista: List [Any], contador: Int = 0): Int = {

    if (lista.isEmpty) {

      -1
    } else {

      val h = lista.head

      h match {
        case h: JButton => {

              if (h.getBackground () == pulsado)
                contador
              else
                buscar_pulsado (lista.tail, contador + 1)
        }
        case _ => buscar_pulsado(lista.tail, contador + 1)
      }

    }

  }


  /**
   * Busca el elemento en la lista y devuelve su índice, o -1 si no se ha encontrado
   */
  def buscar_elem (lista: List [Any], elem: Any, contador: Int = 0): Int = {

    if (lista.isEmpty) {

      -1
    } else {

      if (lista.head == elem)
        contador
      else
        buscar_elem (lista.tail, elem, contador + 1)
    }
  }

  /**
   * Devuelve el color que corresponde con el diamante
   */
  def color (diamante: Any): Color = {

    diamante match {

      case 1 => Color.BLUE
      case 2 => Color.RED
      case 3 => Color.YELLOW
      case 4 => Color.GREEN
      case 5 => Color.CYAN
      case 6 => Color.MAGENTA
      case 7 => Color.PINK
      case 8 => Color.WHITE
      case _ => Color.BLACK
    }
  }
  
  

}

/*
  __  __           _         
 |  \/  |         (_)        
 | \  / |   __ _   _   _ __  
 | |\/| |  / _` | | | | '_ \ 
 | |  | | | (_| | | | | | | |
 |_|  |_|  \__,_| |_| |_| |_|
  
*/
                             

/* -------------------------------------------------------- */
/* Funciones principales del juego                          */
/* -------------------------------------------------------- */

import utils._
import partida._



object Main {

  val msg_menú = "1) Realizar movimiento\n" +
                 "2) Guardar\n" +
                 "3) Cargar\n" +
                 "4) Comprobar huecos\n" +
                 "5) Estrategia óptima\n" +
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
    val l = Utils.crear_lista (partida.filas * partida.columnas, partida.nivel)

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

    val nuevo_estado = eliminar_coicidencias (partida, estado)

    print (msg_menú +
           "--> Introduzca la opción seleccionada: ")

    Utils.pedir_opción (0, 5) match {
      /* Salir */
      case 0 => println (" ---- FIN ---- "); sys.exit (0)
      /* Mover */
      case 1 => menú (partida, movimiento (partida, nuevo_estado))
      /* Guardar */
      case 2 => guardar (partida, nuevo_estado); menú (partida, nuevo_estado)
      /* Cargar */
      case 3 => {

        val datos = cargar

        if (datos == (null, null.asInstanceOf [Int], null)) {

          menú (partida, nuevo_estado)
        } else {

          println ("\n---- Nueva partida ----\n")
          menú (datos._1, (datos._2, datos._3))
        }
      }

      case 4 => menú (partida, eliminar_coicidencias (partida, nuevo_estado))

      case 5 => menú(partida,estrategia_optima(partida,nuevo_estado))
      
      case _ => println ("Opción no reconocida")
    }
  }

  /*Elimina las conincidencias cuando no se realiza ninguna operacion*/
  def eliminar_coicidencias (partida: Partida, estado: (Int,List[Any])): (Int, List [Any])= {

    val matriz = eliminar (estado._2, comprobar_matriz (partida, estado._2))

    println ("*******\n" +
              "Puntos: " + estado._1 + "\n")
    partida.imprimir_matriz (estado._2);

    if (contiene_elemento(0,matriz)){

      println (" ---- \n" +
               "Huecos: ")
      partida.imprimir_matriz (matriz)
      println (" ---- ")

      val matriz_sig = tratar_huecos (partida, matriz)
      val puntos = estado._1 + contar_huecos (matriz)

      eliminar_coicidencias (partida, (puntos, matriz_sig) )
    }
    else
      (estado._1,estado._2)

  }

  /**
   * Función principal para pedir por teclado los datos del movimiento y, tras
   * intercambiar los elementos, realizar las comprobaciones necesarias (tratando los
   * diamantes vacíos).
   *
   * @param partida
   *          Objeto con la información del juego
   *
   * @param estado
   *          Tupla con la puntuación y la matriz de juego (en ese orden)
   *
   *
   * @return
   *          Una nueva tupla de estado con la puntuación y la matriz de juego
   *        actualizadas (en ese orden).
   */
  def movimiento (partida: Partida, estado: (Int, List [Any])): (Int, List [Any]) = {

      val pos: (Int, Int) = pedir_pos (partida)
      val mov = pedir_mov(pos,partida)

      /*Valida el movimiento*/
      if(mov_valido(pos,partida,mov,estado)){

        val l: List [Any] = mover (pos
                                  , partida
                                  , mov
                                  , estado
                            )

        val huecos: List [Any] = eliminar (l, comprobar_matriz (partida, l))
        val puntos: Int = (estado._1 + contar_huecos (huecos))

        val nueva_matriz: List [Any] = tratar_huecos (partida, huecos)

        println (" ---- ")
        println ("Huecos: ")
        partida.imprimir_matriz (huecos)
        println (" ---- ")

        (puntos, nueva_matriz)
    } else {

      println("Este movimiento no es valido\n")
      (estado._1,estado._2)
    }
  }

  /**
   * Rellena los huecos creados al eliminar (poner a 0) las posiciones con coincidencias.
   *
   * @param partida
   *          Objeto con la información del juego
   *
   * @param lista
   *          Matriz de juego
   *
   *
   * @return
   *          Una nueva lista sin diamantes vacíos, rellenando la lista pasada como
   *        argumento según las reglas de la práctica
   */
  def tratar_huecos (partida: Partida, lista: List [Any]): List [Any] = {

    def tratar_huecos_aux_col (partida: Partida
                              , lista: List [Any]
                              , col: Int = 0): List [Any] = {

      if (col >= partida.columnas) {

        lista
      } else {

        val lista_col = Utils.obtener_col (partida, lista, col)

        val salida =  Utils.invertir (
                        tratar_col (Utils.invertir (lista_col)
                                    , partida.nivel
                        )
                      )

        val matriz = Utils.insertar_col (partida, lista, col, salida)

        tratar_huecos_aux_col (partida, matriz, col + 1)
      }
    }

    tratar_huecos_aux_col (partida, lista, 0)
  }

  /**
   * Rellena los huecos que pudiera haber en la columna pasada como argumento.
   **
   * @param columna
   *          Lista con la columna cuyos huecos se deben rellenar. Debe estar en el
   *        orden inverso (es decir, primero el elemento más bajo en la matriz).
   *
   * @param nivel
   *          Nivel del juego para limitar el rango de los diamantes a
   *        crear si es necesario
   *
   *
   * @return
   *          Una nueva lista con la nueva columna, en orden inverso (es decir, la parte
   *        baja de la columna está al principio de la lista devuelta)
   */
  def tratar_col (columna: List [Any], nivel: Int): List [Any] = {

    val elem = columna.head

    if (columna.length == 1) {

      if (elem == 0) Utils.crear_diamante (nivel)::Nil else elem::Nil
    } else {

      if (elem == 0) {

        val cambiado: List [Any] = subir_diamante (columna, nivel)
        cambiado.head::tratar_col (cambiado.tail, nivel)
      } else {

        elem::tratar_col (columna.tail, nivel)
      }
    }
  }

  /**
   * "Sube" el diamante del principio de la lista hasta la posición más alta posible (al
   * final de la lista).
   *
   * @param columna
   *          Lista con el contenido de la columna a tratar.
   *
   * @param nivel
   *          Nivel del juego para limitar el rango de los diamantes a crear
   *
   * @return
   *          Una nueva lista con el hueco rellenado.
   */
  def subir_diamante (columna: List [Any], nivel: Int): List [Any] = {

    val lleno: (Any, Int) = buscar_lleno (columna)

    if (lleno._1 == 0) {

      Utils.crear_diamante (nivel)::columna.tail
    } else {

      Utils.cambiar (columna, 0, lleno._2)
    }
  }

  /**
   * Busca un diamante no vacío (distinto de 0) en la lista.
   *
   * @param lista
   *          Lista en la que busca
   *
   * @param idx
   *          Índice para saber cuántos elementos se han comprobado. Por defecto,
   *        empieza en 0
   *
   *
   * @return
   *          Una tupla con el elemento distinto de 0, si se ha encontrado alguno, y el
   *        índice en el que se encontró.
   */
  def buscar_lleno (lista: List [Any], idx: Int = 0): (Any, Int) = {

    val elem = lista.head

    if (lista.length == 1) {

      (elem, idx)
    } else {

      if (elem != 0) (elem, idx) else buscar_lleno (lista.tail, idx + 1)
    }
  }

  /**
   * Realiza la suma del número de huecos (posiciones a 0) en la lista.
   */
  def contar_huecos (lista: List [Any]): Int = {

    if (lista.length == 0) {

      0
    } else {

      if (lista.head == 0) 1 + contar_huecos (lista.tail) else contar_huecos (lista.tail)
    }
  }

  /**
   * Elimina de la primera matriz los elementos marcados en la segunda.
   *
   * @param lista
   *          Lista con los elementos a ser eliminados (se sustiuyen por 0)
   *
   * @param marcadores
   *          Lista de booleanos con los elementos a eliminar puestos a "true"
   *
   *
   * @return
   *          Una nueva lista con un 0 en las posiciones marcadas.
   */
  def eliminar (lista: List [Any], marcadores: List [Boolean]): List [Any] = {

    val long = marcadores.length

    Utils.mapear_indexado (lista) {
      (e, i) => if ((i < long) && marcadores (i)) 0 else e
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

    println ("\nDatos guardados \n")
  }


  /**
   * Carga una partida que estuviera guardada en un archivo.
   *
   * @return
   *          Una tupla con la información de la partida (en un objeto de tipo Partida),
   *        los puntos y la matriz de juego (en ese orden).
   */
  def cargar: (Partida, Int, List [Any]) = {

    print (" --> Introduzca el nombre del archivo del que cargar los datos: ")
    val nombre = scala.io.StdIn.readLine ()

    val datos = Utils.cargar_partida (nombre)

    if (datos == (null, null.asInstanceOf [Int], null)) {

      println ("\nError al cargar los datos. Compruebe el nombre del fichero.\n")
    } else {

      println ("\nDatos cargados\n")
    }

    datos
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
  * Valida si el movimiento genera una coincidencia de 3 o mas
  *
  * @param elemento
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
  *         Devuelve True si valido y False si no lo es
  */
  def mov_valido(elemento: (Int, Int)
             , partida: Partida
             , movimiento: Int
             , estado: (Int, List [Any])): Boolean= {

    val matriz_mov = mover(elemento,partida,movimiento,estado)
    val matriz_huecos: List [Any] = eliminar(matriz_mov, comprobar_matriz(partida,matriz_mov))

    contiene_elemento(0,matriz_huecos)
  }

  /*Devuelve si contiene el elemento*/
  def contiene_elemento(elemento: Any,lista: List[Any]): Boolean = {

    if(lista.length < 1)
       false
    else
       if(lista.head == elemento)
         true
       else
         contiene_elemento(elemento,lista.tail)
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
      case 0 => Utils.cambiar (estado._2
                              , (elemento._1 * partida.columnas) + elemento._2
                              , ((elemento._1 - 1) * partida.columnas) + elemento._2
                )
      /* Abajo */
      case 1 => Utils.cambiar (estado._2
                              , (elemento._1 * partida.columnas) + elemento._2
                              , ((elemento._1 + 1) * partida.columnas) + elemento._2
                )
      /* Derecha */
      case 2 => Utils.cambiar (estado._2
                              , (elemento._1 * partida.columnas) + elemento._2
                              , (elemento._1 * partida.columnas) + elemento._2 + 1
                )
      /* Izquierda */
      case 3 => Utils.cambiar (estado._2
                              , (elemento._1 * partida.columnas) + elemento._2
                              , (elemento._1 * partida.columnas) + elemento._2 - 1
                )
    }
  }

  /**
   *Busca el movimiento optimo en una posicion dada
   *
   * @param elemento
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
   * @param eliminados 
   * 					Numero de diamantes eliminados
   * 
   * @param movimientof
   * 						
   *
   * @return
   *          Devuelve: Posicion del diamante, movimiento a realizar y el numero de 
   *          diamantes que se eliminan
   */
  def buscar_mov_optimo(elemento: (Int, Int)
            , partida: Partida
            , movimiento: Int
            , estado: (Int, List [Any])
            , eliminados: Int
            , movimientof: Int):((Int,Int),Int,Int) = {
    
    if (movimiento > 3)
      (elemento,movimientof,eliminados)
    else{
      if(/*mov_valido(elemento,partida,movimiento,estado) &&*/ (mov_posible(elemento,partida,movimiento))){
        
        val mov = mover(elemento,partida,movimiento,estado)
        val hue = eliminar (mov, comprobar_matriz (partida, mov))
        val eli = contar_huecos(hue)
        
        //Si elimina mas elementos se cambia
        if(eli > eliminados)
          buscar_mov_optimo(elemento,partida,movimiento+1,estado,eli,movimiento)
        else
          buscar_mov_optimo(elemento,partida,movimiento+1,estado,eliminados,movimientof)
        
      }else
        buscar_mov_optimo(elemento,partida,movimiento+1,estado,eliminados,movimientof)
    }
    
  }
  
  /**
   *Recorre la lsita buscando la jugada optima
   *
   * @param partida
   *          Objeto con la información del juego
   *
   * @param estado
   *          Tupla con la puntuación y la matriz de juego (en ese orden)
   *
   * @param contador 
   * 					Indice para recorrer la lista
   *
   * @param eliminados 
   * 					Numero de diamantes eliminados
   * 
   * @param elememntof
   * 					Tupla con la posicion del elemento en el juego
   * 
   * @param movimientof
   * 						
   *
   * @return
   *          Devuelve: Posicion del diamante y movimiento a realizar 
   */
  def buscar_optimo(partida: Partida
            , estado: (Int, List [Any])
            , contador: Int
            , elementof: (Int,Int)
            , eliminados: Int
            , movimientof: Int):((Int,Int),Int)= {
    
    val indice = estado._2.length - contador
    
    if(contador == 0)
      //Devuelve el resultado final
      
      (elementof,movimientof)
      
    else{
      val jugada : ((Int,Int),Int,Int) = buscar_mov_optimo(obtener_posicion(indice,partida)
          ,partida,0,estado,0,0)
      
          //Caso: Encuentra una mejor jugada
          if(jugada._3 > eliminados)
            buscar_optimo(partida,estado,contador - 1,jugada._1,jugada._3,jugada._2)
          //Caso: Mantine la jugada 
          else
            buscar_optimo(partida,estado,contador - 1,elementof,eliminados,movimientof)
    }
      
     
  }
  
   /**
   *Realiza la jugada optima
   *
   * @param partida
   *          Objeto con la información del juego
   *
   * @param estado
   *          Tupla con la puntuación y la matriz de juego (en ese orden)
   *
   * @return
   *          Devuelve una tupla con la puntuacion y el estado final de la lista
   */
  def estrategia_optima(partida: Partida, estado: (Int, List [Any])):(Int,List[Any]) ={
    
    val tam = estado._2.length
    
    val jugada: ((Int,Int),Int) = buscar_optimo(partida,estado,tam,(0,0),0,0)
    
    val l: List [Any] = mover (jugada._1
                                  , partida
                                  , jugada._2
                                  , estado
                            )

        val huecos: List [Any] = eliminar (l, comprobar_matriz (partida, l))
        val puntos: Int = (estado._1 + contar_huecos (huecos))

        val nueva_matriz: List [Any] = tratar_huecos (partida, huecos)

        println (" ---- ")
        println ("Huecos: ")
        partida.imprimir_matriz (huecos)
        println (" ---- ")

        (puntos, nueva_matriz)
    
    
  }
  /*Trnsforma la posicion de la lista en una posicion de la matriz*/
  def obtener_posicion(indice: Int,partida: Partida):(Int, Int) = {
    val col = indice % partida.columnas
    val fila = (indice - col)/partida.columnas
    (fila,col)
  }
}