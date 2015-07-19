
f = Stop;

foo = (λg x. (λf. x f) g) (λf. g f) ((λg h. f h g) (h f));
