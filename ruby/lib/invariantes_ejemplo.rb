require '../lib/rubyapi'

class Guerrero

  attr_accessor :vida, :fuerza

  invariant { vida >= 0 } #fixme solo se chequea si se cumple para los metodos definidos DESPUES de llamar a inviartiant{}
  invariant { fuerza > 0 && fuerza < 100 }


  def atacar(otro)
    otro.vida -= fuerza
  end

end


g = Guerrero.new
g.vida = -8

