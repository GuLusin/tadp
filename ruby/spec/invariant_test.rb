describe Asd do
  # let(:pila) { Pila.new 7 }

  describe '#materia' do
    it 'debería pasar este test' do
      pila = Pila.new 2
      pila.push 1
      expect(2).to be 1
    end
  end

  describe '#asdasdas' do
    it 'debería romper este test' do
      pila = Pila.new 2
      pila.push 1
      expect(pila.height).to raise_error
    end
  end

end