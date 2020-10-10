require_relative '../rubyapi'

class Operaciones
  #precondición de dividir
  pre { divisor != 0 }
  #postcondición de dividir
  post { |result| result * divisor == dividendo }
  def dividir(dividendo, divisor)
    dividendo / divisor
  end
  # este método no se ve afectado por ninguna pre/post condición
  post { |result| result != 5 }
  def restar(minuendo, sustraendo)
    minuendo - sustraendo
  end

end

ops = Operaciones.new
puts ops.dividir(4, 3.0)
puts ops.dividir(4, 2)

#puts Operaciones.new.dividir(8,0)
puts ops.restar(8,4)