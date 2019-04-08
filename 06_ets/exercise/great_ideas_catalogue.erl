-module(great_ideas_catalogue).

-include_lib("stdlib/include/ms_transform.hrl").

-export([init/0,
         add_idea/5, get_idea/1,
         ideas_by_author/1, ideas_by_rating/1,
         get_authors/0]).

-record(idea, {id, title, author, rating, description}).


init() ->
    ets:new(great_ideas_table, [set, named_table, {keypos, 2}]),
    ets:insert(great_ideas_table,
               [#idea{id = 1, title = "Мороженое с огурцами", author = "Боб Бобов", rating = 100,
                      description = "Крошим огурцы кубиками и добавляем в мороженое"},
                #idea{id = 2, title = "Добыча воды на Марсе", author = "Билл Билов", rating = 500,
                      description = "Бурим скважины на Марсе, доставляем воду на Землю ракетами"},
                #idea{id = 3, title = "Извлечение энергии квазаров", author = "П. И. Шурупов", rating = 100500,
                      description = "Секретно"},
                #idea{id = 4, title = "Куртка с тремя рукавами", author = "Боб Бобов", rating = 15,
                      description = "Рукава из разных материалов, расчитаны на разную погоду."},
                #idea{id = 5, title = "Кроссовки-степлеры", author = "Олечка", rating = 78,
                      description = "Полезная вещь для офиса и фитнеса"},
                #idea{id = 6, title = "Способ ловли кузнечиков", author = "Алекс Аквамаринов", rating = 777,
                      description = "Сачком их, сачком."},
                #idea{id = 7, title = "Вулканический зонт", author = "Боб Бобов", rating = 12,
                      description = "Защищает самолеты от вулканической пыли."},
                #idea{id = 8, title = "Телефон-шар", author = "Див Стобс", rating = 8383,
                      description = "Удобно лежит в руке, имеет устройство ввода на основе гироскопа"},
                #idea{id = 9, title = "Автоматическая кормушка для котов", author = "П. И. Шурупов", rating = 9000,
                      description = "Нужно использовать энергию квазаров для этой цели"},
                #idea{id = 10, title = "Самодвижущаяся лестница", author = "Васисуалий Л.", rating = 42,
                      description = "Имеет большой потенциал применения в небоскребах."}]),
    ok.


add_idea(Id, Title, Author, Rating, Description) ->
    ets:insert(great_ideas_table, [#idea{id = Id, title = Title, author = Author,
              rating = Rating, description = Description}]).


get_idea(Id) ->
  case ets:lookup(great_ideas_table, Id) of
    [State] -> {ok, State};
    [] -> not_found
  end.



ideas_by_author(LAuthor) ->
    Ms = ets:fun2ms(fun(#idea{author = Author} = Think)
      when Author =:= LAuthor -> Think end),
    ets:select(great_ideas_table, Ms).


ideas_by_rating(LRating) ->
  Ms = ets:fun2ms(fun(#idea{rating = Rating} = Think)
    when Rating >= LRating -> Think end),
  ets:select(great_ideas_table, Ms).


get_authors() ->
  Ms = ets:fun2ms(fun(#idea{author = Author}) -> Author end),
  Temp = ets:select(great_ideas_table, Ms),
  F = fun(Name, Acc) ->
      case maps:find(Name, Acc) of
        {ok, Values} -> Acc#{Name :=Values +1};
        error -> Acc#{Name => 1}
      end
    end,
  List = maps:to_list(lists:foldl(F, maps:new(), Temp)),
  lists:sort(fun({A, Q},{B, Q})-> A<B;
  ({_, Q}, {_, W})-> Q>W end, List). %еще надо отсортировать


%%  get_authors([], 0).
%%get_authors(Acc, Flag) when Flag =/= '$end_of_table'->
%%  case Acc of
%%    []-> First = ets:first(great_ideas_table),
%%         [{idea,_,_, Author, _, _}] = ets:lookup(great_ideas_table, First),
%%         get_authors([{Author, 1}], First);
%%    AccT -> Next = ets:next(great_ideas_table, Flag),
%%      Next
%%%%      [{idea,_,_, Author, _, _}] = ets:lookup(great_ideas_table, Next),
%%%%    get_authors([{Author, 1}|AccT], Next)
%%
%%  end.

