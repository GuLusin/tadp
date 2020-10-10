require_relative '../rubyapi'

# crea un avion con una cierta cantidad de asientos y se le puede establecer
# el ratio de combustible gastado por unidad de distancia.
# los pasajeros estaran felices cuando haya comida o el destino incluya 3 o mas vocales

class Avion

  attr_accessor :combustible, :pasajeros, :asientos_totales,:ratio, :pasajeros_felices

  def initialize(asientos_tot, ratio_comb: 1)
    @asientos_totales=asientos_tot
    @combustible=100
    @pasajeros=0
    @ratio=ratio_comb
  end

  invariant { combustible >=0 }
  invariant { pasajeros>=0 }
  invariant { asientos_totales>=pasajeros }

  def cargar_pasajeros(num_pasajeros)
    @pasajeros+=num_pasajeros
  end

  def descargar_pasajeros(num_pasajeros)
    @pasajeros-=num_pasajeros
  end


  pre {cant_pasajeros <= asientos_totales}
  post {@pasajeros_felices==true && pasajeros==0}
  def volar (destino,distancia,cant_pasajeros,hay_comida:false)
    @pasajeros_felices=true if destino.scan(/[aeiouAEIOU]/).count >=3 or hay_comida
    @combustible = @combustible - distancia * @ratio
    cargar_pasajeros(cant_pasajeros)
    descargar_pasajeros(cant_pasajeros)
  end

end



# avion = Avion.new(500)
#
# avion.volar("caca", 50,300,hay_comida: true)