def before_and_after_each_call(pre_cond, post_cond) #TODO
@metodos_redefinidos = []
define_singleton_method :method_added do |metodo_nuevo|
  unless @metodos_redefinidos.include? metodo_nuevo
    @metodos_redefinidos.append metodo_nuevo
    sym_aux_metodos = "#{metodo_nuevo}_aux".to_sym
    @metodos_redefinidos.append sym_aux_metodos
    alias_method(sym_aux_metodos, metodo_nuevo)
    define_method metodo_nuevo do ||
      pre_cond.call
      send(sym_aux_metodos)
      post_cond.call
    end
  end
end
end
