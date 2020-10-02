describe Prueba do
  # let(:pila) { Pila.new 2 }

  describe 'Puedo agregar dos elementos a una pila de capacity = 2' do
    it '' do
      pila = Pila.new 2
      pila.push 1
      expect(pila.height).to be 1
    end
  end

  describe 'Precondicion' do
    it '' do
      pila = Pila.new 2
      pila.push 1
      pila.push 2
      expect { pila.push(5) }.to raise_error 'Failed to meet preconditions'
    end
  end

  describe 'Invariant' do
    it '' do
      expect { Pila.new(-1) }.to raise_error 'Exception invariant estado invalido'
    end
  end
end