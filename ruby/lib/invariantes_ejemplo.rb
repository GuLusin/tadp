require '../lib/rubyapi'

class Guerrero

  attr_accessor :vida, :fuerza

  invariant { 1 == 1 }


  def atacar(otro)
    otro.vida -= fuerza
  end

  def set_vida(num)
    vida = num
  end

end

g = Guerrero.new
g.set_vida(45)

