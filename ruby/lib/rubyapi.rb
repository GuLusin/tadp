# FIXME: esta todo definido para Object, habria que probar definirlo en Module
module Contracts
  def invariant(&invariant_block)
    @invariants ||= []
    @invariants.append invariant_block

    method_redefinition
  end

  def pre(&pre_bloc)
    @pre_condition = pre_bloc

    method_redefinition
  end

  def post(&post_block)
    @post_condition = post_block

    method_redefinition
  end

  private
  def crear_contexto_ejecucion(parametros_metodo, metodo)
    context = clone
    parametros_metodo.zip(metodo.parameters).each do |retorno, valor_parametro|
      context.define_singleton_method(valor_parametro[1]) { retorno }
    end
    context
  end

  private
  def get_invariants
    @invariants ||= []
  end


  private
  def evaluar_condicion(condicion, metodo, argumentos, mensaje_fallo, valor_retorno = nil)
    if condicion
      context = crear_contexto_ejecucion(argumentos, metodo)
      # esto es horrible pero necesario :D
      # Necesito saber si la condicion es "false" y en ese caso explotar, no explotar con falsey's
      raise mensaje_fallo if context.instance_exec(valor_retorno, &condicion) == false
    end
  end

  def before_and_after_each_call_with_invariants(prebloc, postbloc, local_invariants: nil)
    @metodos_redefinidos = []
    define_singleton_method :method_added do |metodo_nuevo|
      if @metodos_redefinidos && !@metodos_redefinidos.include?(metodo_nuevo)
        sym_aux_metodos = "__#{metodo_nuevo}_aux".to_sym

        if prebloc or postbloc
          pre_cond = prebloc
          post_cond = postbloc
        else
          pre_cond = instance_variable_get :@pre_condition if instance_variables.include? :@pre_condition
          post_cond = instance_variable_get :@post_condition if instance_variables.include? :@post_condition
        end

        # puts self

        @metodos_redefinidos.append metodo_nuevo
        @metodos_redefinidos.append sym_aux_metodos
        alias_method(sym_aux_metodos, metodo_nuevo) # TODO: se podria borrar el metodo auxiliar para mantener limpio el objeto

        define_method metodo_nuevo do |*argumentos|

          evaluar_condicion(pre_cond, method(sym_aux_metodos), argumentos, 'Failed to meet preconditions')

          valor_de_retorno = send(sym_aux_metodos, *argumentos)

          evaluar_condicion(post_cond, method(sym_aux_metodos), argumentos, 'Failed to meet postconditions', valor_de_retorno)

          local_invariants&.each do |invariant|
            raise 'Exception invariant estado invalido' unless instance_exec(&invariant)
          end

          valor_de_retorno

        end

        @pre_condition = nil
        @post_condition = nil

      end
    end
  end

  def before_and_after_each_call(preblock,postblock)
    before_and_after_each_call_with_invariants(preblock,postblock,nil)
  end

  def method_redefinition
    before_and_after_each_call_with_invariants(nil, nil, local_invariants: get_invariants)
  end
end

class Object
  include Contracts
end