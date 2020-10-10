require '../lib/rubyapi'

class Guerrero

  attr_accessor :vida, :fuerza

  def initialize
    fuerza = 150
    vida = 100
  end

  invariant { vida >= 0 } #fixme solo se chequea si se cumple para los metodos definidos DESPUES de llamar a inviartiant{}
  invariant { fuerza > 0 && fuerza < 100 }

  def atacar(otro)
    otro.vida = -150
  end

end


g = Guerrero.new
g.vida = -8

f = Guerrero.new
g.atacar(f)
f.atacar(g)


