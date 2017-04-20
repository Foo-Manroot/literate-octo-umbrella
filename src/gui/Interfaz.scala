package interfaz

import javax.swing._   
import java.awt.event._
import java.awt.Color
import JFrame._ 

import partida.Partida
import utils.Utils

object GUI_main {

  def main (args: Array [String]) {

    val partida: Partida = Utils.obtener_args (args)
    val l = Utils.crear_lista (partida.filas * partida.columnas)

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


  def crear_ventana(partida: Partida, estado: (Int, List [Any])): Unit = {
    
    //tamaño de diamante 50 * 50
    val filas = partida.filas
    val columnas = partida.columnas
    
    setDefaultLookAndFeelDecorated(true) 

    setDefaultCloseOperation(EXIT_ON_CLOSE) 

    /*val button = new JButton()
    //getContentPane() add button
    
    button.setText("Funciona")
   
    button addActionListener new ActionListener { 
    def actionPerformed(e : ActionEvent) = 
        Console println "Hello world" 
    }
   */
    val panel: JPanel = new JPanel()
    panel.setSize(columnas * 50,filas * 50)

    Utils.mapear (crear_lista_botones (estado, panel)) { b => panel.add (b) }

    this.setContentPane(panel)
    //pack /*No se que mierdas hace esto*/
   
    setSize(50 + columnas * 50,50 + filas * 50)
    setVisible(true);
    setResizable (false)
  }

  /*Crea una lista con todos los botones*/
  def crear_lista_botones(estado: (Int, List [Any]), panel: JPanel): List[JButton] = { 
    
   val boton = crear_boton(estado._2.head, panel)

   if(estado._2.length == 1) boton::Nil

   else boton::crear_lista_botones((estado._1,estado._2.tail), panel)
  }

  /*Crea un boton segun el identificador*/
  def crear_boton(id: Any, panel: JPanel): JButton = {
    val button = new JButton()
    button.setSize(30, 30)

    button.addActionListener (new ActionListener () {
      def actionPerformed (e: ActionEvent) { controlador (e, panel, button) }
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
  def controlador (e: ActionEvent, panel: JPanel, b: JButton) = {

    val componentes = panel.getComponents ().toList
    val idx_puls: Int = buscar_pulsado (componentes)

    if (idx_puls == -1) {

      b.setBackground (pulsado)
    } else {

      val idx = buscar_elem (componentes, b)
      println ("Actual: " + idx + " - Pulsada: " + idx_puls)
    }

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
