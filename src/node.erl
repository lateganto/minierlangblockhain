-module(node).
-export([main/0, loop/4, start/0]).
-define(TIMEOUT, 10000).

sleep(N) -> receive after N*1000 -> ok end.

watch(Main,Node) ->
  sleep(10),
  Nonce = make_ref(),
  Node ! {ping, self(), Nonce},
  receive
    {pong, Nonce} ->
      %io:format("~p, ~p, Sono ancora vivo!!!!!~n", [Main, Node]),
      watch(Main,Node)
  after 2000 ->
    Main ! {dead, Node}
  %io:format("~p, ~p, Vedi che sono morto!!!!!!~n", [Main, Node])
  end.

loop(Friends, TToMine, TMined, Chain) ->
  case length(Friends) of
    % se non ho amici, chiedo al teacher node
    0 ->
      global:send(teacher_node, {get_friends, self(), make_ref()});

    % se ne ho un numero minore compreso tra 1 e 2 chiedo ad uno dei miei amici
    % TODO avoid loop if there are less then 3 nodes on the net
    N when N<3 ->
      % sceglo a caso uno tra gli amici e mando una richiesta di amicizia
      %io:format("~p, asking friends for new friends~n", [self()]),
      AskToFriend = lists:nth(rand:uniform(length(Friends)), Friends),
      AskToFriend ! {get_friends, self(), make_ref()};
    _ ->
      ok
  end,

  receive
  % se ricevo un ping rispondo con un pong al mittente
    {ping, Sender, Nonce} ->
      Sender ! {pong, Nonce},
      loop(Friends, TToMine, TMined, Chain);

  % se gli altri nodi mi chiedono amici mando semplicemente la lista di amici che ho
    {get_friends, Sender, Nonce} ->
      Sender ! {friends, Nonce, Friends},
      loop(Friends, TToMine, TMined, Chain);

  % il teacher node o gli altri amici mi rispondono con una lista di amici
    {friends, _, Sent_Friends} ->
      % elimino il PID del mio nodo e degli amici (se presenti)
      New_Friends = Sent_Friends -- [self() | Friends],
      % qui viene salvata la nuova lista di amici
      MyNewFriends = addFriends(Friends, New_Friends),
      %io:format("~p My new friends are: ~p~n", [self(), MyNewFriends]),
      loop(MyNewFriends, TToMine, TMined, Chain);

    {dead, Node} ->
      %uno degli amici è morto, ne devo acquisire un altro
      io:format("~p, Dead node ~p~n",[self(), Node]),

      %scelgo uno a caso dei nodi rimanenti e chiedo per nupv amici
      %FriendToAsk = lists:nth(rand:uniform(length(Friends) - 1), Friends -- [Node]),
      %FriendToAsk ! {get_friends, self(), make_ref()},

      %TODO non si eliminano i nodi, perchè?
      loop(Friends -- [Node], TToMine, TMined, Chain);


  %%%%%%%%%%%%%%%%%%%%%%%   TRANSACTION   %%%%%%%%%%%%%%%%%%%%%%%%%%

  % ricevo una transazione {ID, Payload}, se l'ID non è già presente la ritrasmetto agli
  % amici e cerco di inserirla nel prossimo blocco da minare
    {push, T} ->
      NewTransactions =
        case lists:member(T, TToMine) of
          true ->
            %io:format("~p, transazione già presente!~n", [self()]),
            TToMine;
          false ->
            P = spawn(fun() -> sendTransToFriends(T, Friends) end),
            %io:format("~p, invio la transazione a tutti i miei amici con processo ~p!~n", [self(), P]),
            [T | TToMine]
        end,
      loop(Friends, NewTransactions, TMined, Chain);


  %%%%%%%%%%%%%%%%%%%%%%%   CHAIN   %%%%%%%%%%%%%%%%%%%%%%%%%%

    {getChain} ->
      io:format("~p, chiedo la catena agli amici!~n", [self()]),
      case length(Friends) of
        0 ->
          io:format("~p, non ho amici!~n", [self()]),
          loop(Friends, TToMine, TMined, Chain);
        _ ->
          io:format("~p, chiedo la catena ad un amico!~n", [self()]),
          Friend = lists:nth(rand:uniform(length(Friends)), Friends),
          {NewTMined, NewChain} = getChain(Friend, Friends -- [Friend]),
          loop(Friends, TToMine, NewTMined, NewChain)
      end;

    {getHead, Sender, Nonce} ->
      case length(Chain) of
        0 ->
          io:format("~p, la mia catena è vuota, non mando nulla!~n", [self()]),
          Sender ! {head, Nonce, empty};
        _ ->
          io:format("~p, la mia catena non è vuota, mando all'amico la testa!~n", [self()]),
          [H|T] = Chain,
          Sender ! {head, Nonce, H}
      end,
      loop(Friends, TToMine, TMined, Chain);

    {get_previous, Sender, Nonce, IDPreviousBlock} ->
      Block = searchBlock(IDPreviousBlock, Chain),
      case Block of
        none -> stop;
        _ -> Sender ! {previous, Nonce, Block}
      end,
      loop(Friends, TToMine, TMined, Chain);

  %%%%%%%%%%%%%%%%%%%%%%%   UTILS   %%%%%%%%%%%%%%%%%%%%%%%%%%
    {printTTM} ->
      io:format("~p, le transazioni da minare sono: ~p!~n", [self(), TToMine]),
      loop(Friends, TToMine, TMined, Chain);

    {printTM} ->
      io:format("~p, le transazioni minate sono: ~p!~n", [self(), TMined]),
      loop(Friends, TToMine, TMined, Chain);

    {printC} ->
      io:format("~p, le catena è: ~p!~n", [self(), Chain]),
      loop(Friends, TToMine, TMined, Chain);

    {printF} ->
      io:format("~p, i miei amici sono: ~p!~n", [self(), Friends]),
      loop(Friends, TToMine, TMined, Chain)

  end.

% se la lista dei nuovi amici è vuota restituisco la lista di amici vecchia
addFriends(Friends, []) ->
  Friends;

%se la lista di miei amici è vuota devo aggingerne qualcuno
addFriends([], New_Friends) ->
  % scelgo uno tra gli amici a caso
  ToAddFriend = lists:nth(rand:uniform(length(New_Friends)), New_Friends),
  Self = self(),
  spawn(fun() -> watch(Self, ToAddFriend) end),
  addFriends([ToAddFriend], New_Friends -- [ToAddFriend]);

addFriends(Friends, New_Friends) ->
  case length(Friends) < 3 of
    true ->
      ToAddFriend = lists:nth(rand:uniform(length(New_Friends)), New_Friends),
      Self = self(),
      spawn(fun() -> watch(Self, ToAddFriend) end),
      addFriends([ToAddFriend | Friends], New_Friends -- [ToAddFriend]);
    false ->
      Friends
  end.

%%%%%%%%%%%%%%%%%%%%%%%   TRANSACTION   %%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%caso base (chain vuota o blocco non trovato)
sendTransToFriends(T, []) ->
  ok;

sendTransToFriends(T, [F|Friends]) ->
  %io:format("Invio all'amico [~p]~n", [F]),
  F ! {push, T},
  sendTransToFriends(T, Friends -- [F]).

%%%%%%%%%%%%%%%%%%%%%%%%%%   BLOCK   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ho finito di scorrere la catena oppure la catena è vuota e non ho trovato nulla
searchBlock(IDToSearch, []) -> none;

searchBlock(IDToSearch, Chain) ->
  [H|T] = Chain,
  {IDBlock,_,_,_} = H,

  case IDToSearch =:= IDBlock of
    true -> H;
    false -> searchBlock(IDToSearch, T)
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%   CHAIN   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% chiedo la visione della catena ad uno dei miei amici (lo faccio solo all'inizio!)
getChain(Friend, FriendsToAsk) ->
  io:format("~p, chiedo la catena ad un amico a caso!~n", [self()]),
  Friend ! {getHead, self(), make_ref()},
  receive
  % la chain dell'amico è vuota, allora chiedo ad un altro amico
    {head, Nonce, empty} ->
      io:format("~p, la catena dell'amico è vuota!~n", [self()]),
      AnotherFriend = lists:nth(rand:uniform(length(FriendsToAsk)), FriendsToAsk),
      getChain(AnotherFriend, FriendsToAsk -- [AnotherFriend]);

  % a partire dalla testa vado a ricreare tutta la catena
    {head, Nonce, Block} ->
      io:format("~p, la catena c'è, continuo a costruirla!~n", [self()]),
      {_, IDPreviousBlock, Transactions, _} = Block,
      getRemainingChain(Friend, FriendsToAsk, IDPreviousBlock, [Transactions], [Block])

  %se non ricevo alcuna risposta dall'amico per un tot di tempo chiedo ad un altro amico
  after ?TIMEOUT ->
    case length(FriendsToAsk) of
      0 -> [];
      _ ->
        io:format("~p, l'amico non risponde, chiedo ad un altro amico!~n", [self()]),
        AnotherFriend = lists:nth(rand:uniform(length(FriendsToAsk)), FriendsToAsk),
        getChain(AnotherFriend, FriendsToAsk -- [AnotherFriend])
    end
  end.

% caso base in cui sono arrivato all'ultimo blocco della catena e quindi la resituisco insieme 
% alle transazioni minate (presenti nei blocchi della catena)
getRemainingChain(Friend, FriendsToAsk, none, TMined, Chain) ->
  io:format("~p, finito di scorrere la catena, la restituisco!~n", [self()]),
  {TMined, Chain};

getRemainingChain(Friend, FriendsToAsk, IDPreviousBlock, TMined, Chain) ->
  io:format("~p, chiedo all'amico il precedente blocco!~n", [self()]),
  Friend ! {get_previous, self(), make_ref(), IDPreviousBlock},
  receive
    {previous, Nonce, Block} ->
      io:format("~p, blocco precedente ricevuto!~n", [self()]),
      {_, IDPreviousBlockTemp, Transactions, _} = Block,
      Friend ! {get_previous, self(), make_ref(), IDPreviousBlockTemp},
      getRemainingChain(Friend, FriendsToAsk, IDPreviousBlockTemp, TMined++Transactions, Chain++[Block])

  %se non ricevo risposta dopo un certo intervallo di tempo rispondo
  after ?TIMEOUT ->
    case length(FriendsToAsk) of
      0 -> do_nothing;
      _ ->
        AnotherFriend = lists:nth(rand:uniform(length(FriendsToAsk)), FriendsToAsk),
        getRemainingChain(AnotherFriend, FriendsToAsk -- [AnotherFriend], IDPreviousBlock, TMined, Chain)
    end
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start() ->
  % invio una prima richiesta al teacher node per registrarmi
  global:send(teacher_node, {get_friends, self(), make_ref()}),
  %io:format("~p, ask to teacher_node for initial friends!~n", [self()]),
  sleep(10),

  io:format("~p, CHIEDO PER CATENA!~n", [self()]),
  self() ! {getChain},

  % avvio il ciclo
  loop([],[],[],[]).


main() ->
  % start a new process for teacher and node
  %T = spawn(teacher_node, main, []),
  %sleep(5),
  %N1 = spawn(node, loop, [[],[]]),
  %N2 = spawn(node, loop, [[],[]]),
  %N3 = spawn(node, loop, [[],[]]),
  %N4 = spawn(node, loop, [[],[]]).



  %T = spawn(teacher_node, main, []),
  %Tra = [a1,a2,a3,b1,b2,b3,c1,c2,c3],
  %Cha = [{3,2,[c1,c2,c3],1234}, {2,1,[b1,b2,b3],1234}, {1,none,[a1,a2,a3],1234}],
  %N1 = spawn(node, loop, [[],[],Tra, Cha]),
  %N1 = spawn(node, loop, [[],[],[],[]]).
  %global:send(teacher_node, {get_friends, N1, make_ref()}),

  T = spawn(teacher_node, main, []),
  sleep(5),
  Tra = [a1,a2,a3,b1,b2,b3,c1,c2,c3],
  Cha = [{3,2,[c1,c2,c3],1234}, {2,1,[b1,b2,b3],1234}, {1,none,[a1,a2,a3],1234}],
  N1 = spawn(node, loop, [[],[],Tra, Cha]),
  %N1 = spawn(node, start, []),
  sleep(5),
  N2 = spawn(node, start, []),
  N3 = spawn(node, start, []),
  N4 = spawn(node, start, []).

