(ns watcompiler.re
  (:require [clojure.set :refer :all]
            [watcompiler.nfa :refer :all]
            [watcompiler.lang :refer :all])
  (:import [watcompiler.nfa NFA]))

;; Helpers for merging nfas
(defn merge-nfas
  [& args]
  (apply merge args))

(defn merge-nfas-states
  [& args]
  (apply union (:states args)))

(defn merge-nfas-accept-states
  [& args]
  (apply merge (:accept-states args)))

(defn merge-nfas-transitions
  [& args]
  (apply merge (:transitions args)))

(defn merge-nfas-accept-priorities
  [& args]
  (apply merge (:accept-priorities args)))

(def integer-literal-nfa
  (let [stateS (gensym :S)
        state1 (gensym :1)
        state2 (gensym :2)]
  (make-NFA (into #{} )
            #{stateS state1 state2}
            stateS
            {state2 (list :INTEGER 0)}
            (make-transition-NFA [[stateS state1 e]
                                  [state1 state2 DIGITS-NONZERO]
                                  [state2 state2 DIGITS]]))))
;; Operators
(def operators-nfa
  (let [stateS (gensym :S)
        state1 (gensym :1) ;; valid, but can add =
        state2 (gensym :2) ;; valid, but can add <, =
        state3 (gensym :3) ;; valid but can add >, =
        state4 (gensym :4) ;; valid, but can add +
        state5 (gensym :5) ;; valid, but can add -
        state6 (gensym :6) ;; valid, but can add >, =
        state7 (gensym :7) ;; valid, but can add >, =
        state11 (gensym :11)] ;; Nothing else can be added to it
  (make-NFA (into #{} )
            #{stateS state1 state2, state3, state4, state5, state6, state11}
            stateS
            {state11 (list :OPERATOR 0)
             state1 (list :OPERATOR 1)
             state2 (list :OPERATOR 2)
             state3 (list :OPERATOR 3)
             state4 (list :OPERATOR 4)
             state5 (list :OPERATOR 5)
             state6 (list :OPERATOR 6)
             state7 (list :OPERATOR 7)}
            (make-transition-NFA [[stateS state1 \=]
                                  [stateS state2 \<]
                                  [stateS state3 \>]
                                  [stateS state1 \!]
                                  [stateS state1 \:]
                                  [stateS state1 \~]
                                  [stateS state1 \?]
                                  [stateS state1 \&]
                                  [stateS state1 \|]
                                  [stateS state1 \^]
                                  [stateS state1 \%]
                                  [stateS state4 \+]
                                  [stateS state5 \-]
                                  [stateS state1 \*]
                                  [stateS state1 \/]

                                  [state1 state11 \=]
                                  [state2 state1 \<]
                                  [state3 state6 \>]
                                  [state3 state11 \=]
                                  [state4 state11 \+]
                                  [state5 state11 \-]
                                  [state6 state7 \>] ;; >>
                                  [state6 state11 \=] ;; >>=
                                  [state7 state11 \=] ;; >>>
                                  ]))))

;; Keywords
;; From page 46 of jls2.pdf
(def abstract-nfa
  (let [stateS    (gensym :S)
        a         (gensym :a)
        ab        (gensym :ab)
        abs       (gensym :abs)
        abst      (gensym :abst)
        abstr     (gensym :abstr)
        abstra    (gensym :abstra)
        abstrac   (gensym :abstrac)
        abstract  (gensym :abstract)]
  (make-NFA (into #{} )
            #{stateS a ab abs abst abstr abstra abstrac abstract}
            stateS
            {abstract (list :KEYWORD 0)}
            (make-transition-NFA [[stateS     a        \a]
                                  [a          ab       \b]
                                  [ab         abs      \s]
                                  [abs        abst     \t]
                                  [abst       abstr    \r]
                                  [abstr      abstra   \a]
                                  [abstra     abstrac  \c]
                                  [abstrac    abstract \t]]))))

(def default-nfa
  (let [stateS    (gensym :S)
        d         (gensym :d)
        de        (gensym :de)
        def       (gensym :def)
        defa      (gensym :defa)
        defau     (gensym :defau)
        defaul    (gensym :defaul)
        default   (gensym :default)]
  (make-NFA (into #{} )
            #{stateS d de def defa defau defaul default}
            stateS
            {default (list :KEYWORD 0)}
            (make-transition-NFA [[stateS     d        \d]
                                  [d          de       \e]
                                  [de         def      \f]
                                  [def        defa     \a]
                                  [defa       defau    \u]
                                  [defau      defaul   \l]
                                  [defaul     default  \t]]))))

(def if-nfa
  (let [stateS   (gensym :S)
        i        (gensym :i)
        if       (gensym :if)]
  (make-NFA (into #{} )
            #{stateS i if}
            stateS
            {if (list :KEYWORD 0)}
            (make-transition-NFA [[stateS i  \i]
                                  [i      if \f]]))))

(def private-nfa
  (let [stateS    (gensym :S)
        p         (gensym :p)
        pr        (gensym :pr)
        pri       (gensym :pri)
        priv      (gensym :priv)
        priva     (gensym :priva)
        privat    (gensym :privat)
        private   (gensym :private)]
  (make-NFA (into #{} )
            #{stateS p pr pri priv priva privat private}
            stateS
            {private (list :KEYWORD 0)}
            (make-transition-NFA [[stateS     p        \p]
                                  [p          pr       \r]
                                  [pr         pri      \i]
                                  [pri        priv     \v]
                                  [priv       priva    \a]
                                  [priva      privat   \t]
                                  [privat     private  \e]]))))

(def this-nfa
  (let [stateS   (gensym :S)
        t        (gensym :t)
        th       (gensym :th)
        thi      (gensym :thi)
        this     (gensym :this)]
  (make-NFA (into #{} )
            #{stateS t th thi this}
            stateS
            {this (list :KEYWORD 0)}
            (make-transition-NFA [[stateS  t    \t]
                                  [t       th   \h]
                                  [th      thi  \i]
                                  [thi     this \s]]))))

(def boolean-nfa
  (let [stateS    (gensym :S)
        b         (gensym :b)
        bo        (gensym :bo)
        boo       (gensym :boo)
        bool      (gensym :bool)
        boole     (gensym :boole)
        boolea    (gensym :boolea)
        boolean   (gensym :boolean)]
  (make-NFA (into #{} )
            #{stateS b bo boo bool boole boolea boolean}
            stateS
            {boolean (list :KEYWORD 0)}
            (make-transition-NFA [[stateS     b        \b]
                                  [b          bo       \o]
                                  [bo         boo      \o]
                                  [boo        bool     \l]
                                  [bool       boole    \e]
                                  [boole      boolea   \a]
                                  [boolea     boolean  \n]]))))

(def do-nfa
  (let [stateS   (gensym :S)
        d        (gensym :d)
        do       (gensym :do)]
  (make-NFA (into #{} )
            #{stateS d do}
            stateS
            {do (list :KEYWORD 0)}
            (make-transition-NFA [[stateS d  \d]
                                  [d      do \o]]))))

(def implements-nfa
  (let [stateS      (gensym :S)
        i           (gensym :i)
        im          (gensym :im)
        imp         (gensym :imp)
        impl        (gensym :impl)
        imple       (gensym :imple)
        implem      (gensym :implem)
        impleme     (gensym :impleme)
        implemen    (gensym :implemen)
        implement   (gensym :implement)
        implements  (gensym :implements)]
  (make-NFA (into #{} )
            #{stateS i im imp impl imple implem impleme implemen implement implements}
            stateS
            {implements (list :KEYWORD 0)}
            (make-transition-NFA [[stateS       i          \i]
                                  [i            im         \m]
                                  [im           imp        \p]
                                  [imp          impl       \l]
                                  [impl         imple      \e]
                                  [imple        implem     \m]
                                  [implem       impleme    \e]
                                  [impleme      implemen   \n]
                                  [implemen     implement  \t]
                                  [implement    implements \s]]))))

(def protected-nfa
  (let [stateS      (gensym :S)
        p           (gensym :p)
        pr          (gensym :pr)
        pro         (gensym :pro)
        prot        (gensym :prot)
        prote       (gensym :prote)
        protec      (gensym :protec)
        protect     (gensym :protect)
        protecte    (gensym :protecte)
        protected   (gensym :protected)]
  (make-NFA (into #{} )
            #{stateS p pr pro prot prote protec protect protecte protected}
            stateS
            {protected (list :KEYWORD 0)}
            (make-transition-NFA [[stateS       p          \p]
                                  [p            pr         \r]
                                  [pr           pro        \o]
                                  [pro          prot       \t]
                                  [prot         prote      \e]
                                  [prote        protec     \c]
                                  [protec       protect    \t]
                                  [protect      protecte   \e]
                                  [protecte     protected  \d]]))))

;; throw and throws
(def throws-nfa
  (let [stateS    (gensym :S)
        t         (gensym :t)
        th        (gensym :th)
        thr       (gensym :thr)
        thro      (gensym :thro)
        throw     (gensym :throw)
        throws    (gensym :throws)]
  (make-NFA (into #{} )
            #{stateS t th thr thro throw throws}
            stateS
            {throws (list :KEYWORD 0)
             throw  (list :KEYWORD 1)}
            (make-transition-NFA [[stateS     t        \t]
                                  [t          th       \h]
                                  [th         thr      \r]
                                  [thr        thro     \o]
                                  [thro       throw    \w]
                                  [throw      throws   \s]]))))

(def break-nfa
  (let [stateS    (gensym :S)
        b         (gensym :b)
        br        (gensym :br)
        bre       (gensym :bre)
        brea      (gensym :brea)
        break     (gensym :break)]
  (make-NFA (into #{} )
            #{stateS b br bre brea break}
            stateS
            {break (list :KEYWORD 0)}
            (make-transition-NFA [[stateS     b        \b]
                                  [b          br       \r]
                                  [br         bre      \e]
                                  [bre        brea     \a]
                                  [brea       break    \k]]))))

(def double-nfa
  (let [stateS    (gensym :S)
        d         (gensym :d)
        do        (gensym :do)
        dou       (gensym :dou)
        doub      (gensym :doub)
        doubl     (gensym :doubl)
        double    (gensym :double)]
  (make-NFA (into #{} )
            #{stateS d do dou doub doubl double}
            stateS
            {double (list :KEYWORD 0)}
            (make-transition-NFA [[stateS     d        \d]
                                  [d          do       \o]
                                  [do         dou      \u]
                                  [dou        doub     \b]
                                  [doub       doubl    \l]
                                  [double     double   \e]]))))

(def import-nfa
  (let [stateS    (gensym :S)
        i         (gensym :i)
        im        (gensym :im)
        imp       (gensym :imp)
        impo      (gensym :impo)
        impor     (gensym :impor)
        import    (gensym :import)]
  (make-NFA (into #{} )
            #{stateS i im imp impo impor import}
            stateS
            {import (list :KEYWORD 0)}
            (make-transition-NFA [[stateS     i        \i]
                                  [i          im       \m]
                                  [im         imp      \p]
                                  [imp        impo     \o]
                                  [impo       impor    \r]
                                  [impor      import   \t]]))))

(def public-nfa
  (let [stateS    (gensym :S)
        p         (gensym :p)
        pu        (gensym :pu)
        pub       (gensym :pub)
        publ      (gensym :publ)
        publi     (gensym :publi)
        public    (gensym :public)]
  (make-NFA (into #{} )
            #{stateS p pu pub publ publi public}
            stateS
            {public (list :KEYWORD 0)}
            (make-transition-NFA [[stateS     p        \p]
                                  [p          pu       \u]
                                  [pu         pub      \b]
                                  [pub        publ     \l]
                                  [publ       publi    \i]
                                  [publi      public   \c]]))))

(def byte-nfa
  (let [stateS   (gensym :S)
        b        (gensym :b)
        by       (gensym :by)
        byt      (gensym :byt)
        byte     (gensym :byte)]
  (make-NFA (into #{} )
            #{stateS b by byt byte}
            stateS
            {byte (list :KEYWORD 0)}
            (make-transition-NFA [[stateS  b    \b]
                                  [b       by   \y]
                                  [by      byt  \t]
                                  [byt     byte \e]]))))

(def else-nfa
  (let [stateS   (gensym :S)
        e        (gensym :e)
        el       (gensym :el)
        els      (gensym :els)
        else     (gensym :else)]
  (make-NFA (into #{} )
            #{stateS e el els else}
            stateS
            {else (list :KEYWORD 0)}
            (make-transition-NFA [[stateS  e    \e]
                                  [e       el   \l]
                                  [el      els  \s]
                                  [els     else \e]]))))

(def instanceof-nfa
  (let [stateS      (gensym :S)
        i           (gensym :i)
        in          (gensym :in)
        ins         (gensym :ins)
        inst        (gensym :inst)
        insta       (gensym :insta)
        instan      (gensym :instan)
        instanc     (gensym :instanc)
        instance    (gensym :instance)
        instanceo   (gensym :instanceo)
        instanceof  (gensym :instanceof)]
  (make-NFA (into #{} )
            #{stateS i in ins inst insta instan instanc instance instanceo instanceof}
            stateS
            {instanceof (list :KEYWORD 0)}
            (make-transition-NFA [[stateS       i          \i]
                                  [i            in         \n]
                                  [in           ins        \s]
                                  [ins          inst       \t]
                                  [inst         insta      \a]
                                  [insta        instan     \n]
                                  [instan       instanc    \c]
                                  [instanc      instance   \e]
                                  [instance     instanceo  \o]
                                  [instanceo    instanceof \f]]))))

(def return-nfa
  (let [stateS    (gensym :S)
        r         (gensym :r)
        re        (gensym :re)
        ret       (gensym :ret)
        retu      (gensym :retu)
        retur     (gensym :retur)
        return    (gensym :return)]
  (make-NFA (into #{} )
            #{stateS r re ret retu retur return}
            stateS
            {return (list :KEYWORD 0)}
            (make-transition-NFA [[stateS     r        \r]
                                  [r          re       \e]
                                  [re         ret      \t]
                                  [ret        retu     \u]
                                  [retu       retur    \r]
                                  [retur      return   \n]]))))

(def transient-nfa
  (let [stateS      (gensym :S)
        t           (gensym :t)
        tr          (gensym :tr)
        tra         (gensym :tra)
        tran        (gensym :tran)
        trans       (gensym :trans)
        transi      (gensym :transi)
        transie     (gensym :transie)
        transien    (gensym :transien)
        transient   (gensym :transient)]
  (make-NFA (into #{} )
            #{stateS t tr tra tran trans transi transie transien transient}
            stateS
            {transient (list :KEYWORD 0)}
            (make-transition-NFA [[stateS       t          \t]
                                  [t            tr         \r]
                                  [tr           tra        \a]
                                  [tra          tran       \n]
                                  [tran         trans      \s]
                                  [trans        transi     \i]
                                  [transi       transie    \e]
                                  [transie      transien   \n]
                                  [transien     transient  \t]]))))

(def case-nfa
  (let [stateS   (gensym :S)
        c        (gensym :c)
        ca       (gensym :ca)
        cas      (gensym :cas)
        case     (gensym :case)]
  (make-NFA (into #{} )
            #{stateS c ca cas case}
            stateS
            {case (list :KEYWORD 0)}
            (make-transition-NFA [[stateS  c    \c]
                                  [c       ca   \a]
                                  [ca      cas  \s]
                                  [cas     case \e]]))))

(def extends-nfa
  (let [stateS    (gensym :S)
        e         (gensym :e)
        ex        (gensym :ex)
        ext       (gensym :ext)
        exte      (gensym :exte)
        exten     (gensym :exten)
        extend    (gensym :extend)
        extends   (gensym :extends)]
  (make-NFA (into #{} )
            #{stateS e ex ext exte exten extend extends}
            stateS
            {extends (list :KEYWORD 0)}
            (make-transition-NFA [[stateS     e        \e]
                                  [e          ex       \x]
                                  [ex         ext      \t]
                                  [ext        exte     \e]
                                  [exte       exten    \n]
                                  [exten      extend   \d]
                                  [extend     extends  \s]]))))

(def int-nfa
  (let [stateS   (gensym :S)
        i        (gensym :i)
        in       (gensym :in)
        int      (gensym :int)]
  (make-NFA (into #{} )
            #{stateS i in int}
            stateS
            {int (list :KEYWORD 0)}
            (make-transition-NFA [[stateS  i   \i]
                                  [i       in  \n]
                                  [in      int \t]]))))

(def short-nfa
  (let [stateS    (gensym :S)
        s         (gensym :s)
        sh        (gensym :sh)
        sho       (gensym :sho)
        shor      (gensym :shor)
        short     (gensym :short)]
  (make-NFA (into #{} )
            #{stateS s sh sho shor short}
            stateS
            {short (list :KEYWORD 0)}
            (make-transition-NFA [[stateS     s        \s]
                                  [s          sh       \h]
                                  [sh         sho      \o]
                                  [sho        shor     \r]
                                  [shor       short    \t]]))))

(def try-nfa
  (let [stateS   (gensym :S)
        t        (gensym :t)
        tr       (gensym :tr)
        try      (gensym :try)]
  (make-NFA (into #{} )
            #{stateS t tr try}
            stateS
            {try (list :KEYWORD 0)}
            (make-transition-NFA [[stateS  t   \t]
                                  [t       tr  \r]
                                  [tr      try \y]]))))

(def catch-nfa
  (let [stateS    (gensym :S)
        c         (gensym :c)
        ca        (gensym :ca)
        cat       (gensym :cat)
        catc      (gensym :catc)
        catch     (gensym :catch)]
  (make-NFA (into #{} )
            #{stateS c ca cat catc catch}
            stateS
            {catch (list :KEYWORD 0)}
            (make-transition-NFA [[stateS     c        \c]
                                  [c          ca       \a]
                                  [ca         cat      \t]
                                  [cat        catc     \c]
                                  [catc       catch    \h]]))))

;; final and finally
(def finally-nfa
  (let [stateS    (gensym :S)
        f         (gensym :f)
        fi        (gensym :fi)
        fin       (gensym :fin)
        fina      (gensym :fina)
        final     (gensym :final)
        finall    (gensym :finall)
        finally   (gensym :finally)]
  (make-NFA (into #{} )
            #{stateS f fi fin fina final finall finally}
            stateS
            {finally (list :KEYWORD 0)
             final   (list :KEYWORD 1)}
            (make-transition-NFA [[stateS     f        \f]
                                  [f          fi       \i]
                                  [fi         fin      \n]
                                  [fin        fina     \a]
                                  [fina       final    \l]
                                  [final      finall   \l]
                                  [finall     finally  \y]]))))

(def interface-nfa
  (let [stateS      (gensym :S)
        i           (gensym :i)
        in          (gensym :in)
        int         (gensym :int)
        inte        (gensym :inte)
        inter       (gensym :inter)
        interf      (gensym :interf)
        interfa     (gensym :interfa)
        interfac    (gensym :interfac)
        interface   (gensym :interface)]
  (make-NFA (into #{} )
            #{stateS i in int inte inter interf interfa interfac interface}
            stateS
            {interface (list :KEYWORD 0)}
            (make-transition-NFA [[stateS       i          \i]
                                  [i            in         \n]
                                  [in           int        \t]
                                  [int          inte       \e]
                                  [inte         inter      \r]
                                  [inter        interf     \f]
                                  [interf       interfa    \a]
                                  [interfa      interfac   \c]
                                  [interfac     interface  \e]]))))

(def static-nfa
  (let [stateS    (gensym :S)
        s         (gensym :s)
        st        (gensym :st)
        sta       (gensym :sta)
        stat      (gensym :stat)
        stati     (gensym :stati)
        static    (gensym :static)]
  (make-NFA (into #{} )
            #{stateS static}
            stateS
            {static (list :KEYWORD 0)}
            (make-transition-NFA [[stateS     s        \s]
                                  [s          st       \t]
                                  [st         sta      \a]
                                  [sta        stat     \t]
                                  [stat       stati    \i]
                                  [stati      static   \c]]))))

(def void-nfa
  (let [stateS   (gensym :S)
        v        (gensym :v)
        vo       (gensym :vo)
        voi      (gensym :voi)
        void     (gensym :void)]
  (make-NFA (into #{} )
            #{stateS v vo voi void}
            stateS
            {void (list :KEYWORD 0)}
            (make-transition-NFA [[stateS  v    \v]
                                  [v       vo   \o]
                                  [vo      voi  \i]
                                  [voi     void \d]]))))

(def char-nfa
  (let [stateS   (gensym :S)
        c        (gensym :c)
        ch       (gensym :ch)
        cha      (gensym :cha)
        char     (gensym :char)]
  (make-NFA (into #{} )
            #{stateS c ch cha char}
            stateS
            {char (list :KEYWORD 0)}
            (make-transition-NFA [[stateS  c    \c]
                                  [c       ch   \h]
                                  [ch      cha  \a]
                                  [cha     char \r]]))))

(def long-nfa
  (let [stateS   (gensym :S)
        l        (gensym :l)
        lo       (gensym :lo)
        lon      (gensym :lon)
        long     (gensym :long)]
  (make-NFA (into #{} )
            #{stateS l lo lon long}
            stateS
            {long (list :KEYWORD 0)}
            (make-transition-NFA [[stateS  l    \l]
                                  [l       lo   \o]
                                  [lo      lon  \n]
                                  [lon     long \g]]))))

(def strictfp-nfa
  (let [stateS    (gensym :S)
        s         (gensym :s)
        st        (gensym :st)
        str       (gensym :str)
        stri      (gensym :stri)
        stric     (gensym :stric)
        strict    (gensym :strict)
        strictf   (gensym :strictf)
        strictfp  (gensym :strictfp)]
  (make-NFA (into #{} )
            #{stateS s st str stri stric strict strictf strictfp}
            stateS
            {strictfp (list :KEYWORD 0)}
            (make-transition-NFA [[stateS     s        \s]
                                  [s          st       \t]
                                  [st         str      \r]
                                  [str        stri     \i]
                                  [stri       stric    \c]
                                  [stric      strict   \t]
                                  [strict     strictf  \f]
                                  [strictf    strictfp \p]]))))

(def volatile-nfa
  (let [stateS    (gensym :S)
        v         (gensym :v)
        vo        (gensym :vo)
        vol       (gensym :vol)
        vola      (gensym :vola)
        volat     (gensym :volat)
        volati    (gensym :volati)
        volatil   (gensym :volatil)
        volatile  (gensym :volatile)]
  (make-NFA (into #{} )
            #{stateS v vo vol vola volat volati volatil volatile}
            stateS
            {volatile (list :KEYWORD 0)}
            (make-transition-NFA [[stateS     v        \v]
                                  [v          vo       \o]
                                  [vo         vol      \l]
                                  [vol        vola     \a]
                                  [vola       volat    \t]
                                  [volat      volati   \i]
                                  [volati     volatil  \l]
                                  [volatil    volatile \e]]))))

(def class-nfa
  (let [stateS    (gensym :S)
        c         (gensym :c)
        cl        (gensym :cl)
        cla       (gensym :cla)
        clas      (gensym :clas)
        class     (gensym :class)]
  (make-NFA (into #{} )
            #{stateS c cl cla clas class}
            stateS
            {class (list :KEYWORD 0)}
            (make-transition-NFA [[stateS     c        \c]
                                  [c          cl       \l]
                                  [cl         cla      \a]
                                  [cla        clas     \s]
                                  [clas       class    \s]]))))

(def float-nfa
  (let [stateS    (gensym :S)
        f         (gensym :f)
        fl        (gensym :fl)
        flo       (gensym :flo)
        floa      (gensym :floa)
        float     (gensym :float)]
  (make-NFA (into #{} )
            #{stateS f fl flo floa float}
            stateS
            {float (list :KEYWORD 0)}
            (make-transition-NFA [[stateS     f        \f]
                                  [f          fl       \l]
                                  [fl         flo      \o]
                                  [flo        floa     \a]
                                  [floa       float    \t]]))))

(def native-nfa
  (let [stateS    (gensym :S)
        n         (gensym :n)
        na        (gensym :na)
        nat       (gensym :nat)
        nati      (gensym :nati)
        nativ     (gensym :nativ)
        native    (gensym :native)]
  (make-NFA (into #{} )
            #{stateS n na nat nati nativ native}
            stateS
            {native (list :KEYWORD 0)}
            (make-transition-NFA [[stateS     n        \n]
                                  [n          na       \a]
                                  [na         nat      \t]
                                  [nat        nati     \i]
                                  [nati       nativ    \v]
                                  [nativ      native   \e]]))))

(def super-nfa
  (let [stateS    (gensym :S)
        s         (gensym :s)
        su        (gensym :su)
        sup       (gensym :sup)
        supe      (gensym :supe)
        super     (gensym :super)]
  (make-NFA (into #{} )
            #{stateS s su sup supe super}
            stateS
            {super (list :KEYWORD 0)}
            (make-transition-NFA [[stateS     s        \s]
                                  [s          su       \u]
                                  [su         sup      \p]
                                  [sup        supe     \e]
                                  [supe       super    \r]]))))

(def while-nfa
  (let [stateS    (gensym :S)
        w         (gensym :w)
        wh        (gensym :wh)
        whi       (gensym :whi)
        whil      (gensym :whil)
        while     (gensym :while)]
  (make-NFA (into #{} )
            #{stateS w wh whi whil while}
            stateS
            {while (list :KEYWORD 0)}
            (make-transition-NFA [[stateS     w        \w]
                                  [w          wh       \h]
                                  [wh         whi      \i]
                                  [whi        whil     \l]
                                  [whil       while    \e]]))))

(def const-nfa
  (let [stateS    (gensym :S)
        c         (gensym :c)
        co        (gensym :co)
        con       (gensym :con)
        cons      (gensym :cons)
        const     (gensym :const)]
  (make-NFA (into #{} )
            #{stateS c co con cons const}
            stateS
            {const (list :KEYWORD 0)}
            (make-transition-NFA [[stateS     c        \c]
                                  [c          co       \o]
                                  [co         con      \n]
                                  [con        cons     \s]
                                  [cons       const    \t]]))))

(def for-nfa
  (let [stateS   (gensym :S)
        f        (gensym :f)
        fo       (gensym :fo)
        for      (gensym :for)]
  (make-NFA (into #{} )
            #{stateS f fo for}
            stateS
            {for (list :KEYWORD 0)}
            (make-transition-NFA [[stateS  f   \f]
                                  [f       fo  \o]
                                  [fo      for \r]]))))

(def new-nfa
  (let [stateS   (gensym :S)
        n        (gensym :n)
        ne       (gensym :ne)
        new      (gensym :new)]
  (make-NFA (into #{} )
            #{stateS n ne new}
            stateS
            {new (list :KEYWORD 0)}
            (make-transition-NFA [[stateS  n   \n]
                                  [n       ne  \e]
                                  [ne      new \w]]))))

(def switch-nfa
  (let [stateS    (gensym :S)
        s         (gensym :s)
        sw        (gensym :sw)
        swi       (gensym :swi)
        swit      (gensym :swit)
        switc     (gensym :switc)
        switch    (gensym :switch)]
  (make-NFA (into #{} )
            #{stateS s sw swi swit switc switch}
            stateS
            {switch (list :KEYWORD 0)}
            (make-transition-NFA [[stateS     s        \s]
                                  [s          sw       \w]
                                  [sw         swi      \i]
                                  [swi        swit     \t]
                                  [swit       switc    \c]
                                  [switc      switch   \h]]))))

(def continue-nfa
  (let [stateS    (gensym :S)
        c         (gensym :c)
        co        (gensym :co)
        con       (gensym :con)
        cont      (gensym :cont)
        conti     (gensym :conti)
        contin    (gensym :contin)
        continu   (gensym :continu)
        continue  (gensym :continue)]
  (make-NFA (into #{} )
            #{stateS c co con cont conti contin continu continue}
            stateS
            {continue (list :KEYWORD 0)}
            (make-transition-NFA [[stateS     c        \c]
                                  [c          co       \o]
                                  [co         con      \n]
                                  [con        cont     \t]
                                  [cont       conti    \i]
                                  [conti      contin   \n]
                                  [contin     continu  \u]
                                  [continu    continue \e]]))))

(def goto-nfa
  (let [stateS   (gensym :S)
        g        (gensym :g)
        go       (gensym :go)
        got      (gensym :got)
        goto     (gensym :goto)]
  (make-NFA (into #{} )
            #{stateS g go got goto}
            stateS
            {goto (list :KEYWORD 0)}
            (make-transition-NFA [[stateS  g    \g]
                                  [g       go   \o]
                                  [go      got  \t]
                                  [got     goto \o]]))))

(def package-nfa
  (let [stateS    (gensym :S)
        p         (gensym :p)
        pa        (gensym :pa)
        pac       (gensym :pac)
        pack      (gensym :pack)
        packa     (gensym :packa)
        packag    (gensym :packag)
        package   (gensym :package)]
  (make-NFA (into #{} )
            #{stateS p pa pac pack packa packag package}
            stateS
            {package (list :KEYWORD 0)}
            (make-transition-NFA [[stateS     p        \p]
                                  [p          pa       \a]
                                  [pa         pac      \c]
                                  [pac        pack     \k]
                                  [pack       packa    \a]
                                  [packa      packag   \g]
                                  [packag     package  \e]]))))

(def synchronized-nfa
  (let [stateS        (gensym :S)
        s             (gensym :s)
        sy            (gensym :sy)
        syn           (gensym :syn)
        sync          (gensym :sync)
        synch         (gensym :synch)
        synchr        (gensym :synchr)
        synchro       (gensym :synchro)
        synchron      (gensym :synchron)
        synchroni     (gensym :synchroni)
        synchroniz    (gensym :synchroniz)
        synchronize   (gensym :synchronize)
        synchronized  (gensym :synchronized)]
  (make-NFA (into #{} )
            #{stateS s sy syn sync synch synchr synchro synchron synchroni synchroniz synchronize synchronized}
            stateS
            {synchronized (list :KEYWORD 0)}
            (make-transition-NFA [[stateS         s            \s]
                                  [s              sy           \y]
                                  [sy             syn          \n]
                                  [syn            sync         \c]
                                  [sync           synch        \h]
                                  [synch          synchr       \r]
                                  [synchro        synchro      \o]
                                  [synchron       synchron     \n]
                                  [synchroni      synchroni    \i]
                                  [synchroniz     synchroniz   \z]
                                  [synchronize    synchronize  \e]
                                  [synchronized   synchronized \d]]))))

(def keywords-nfa
  (let [stateS (gensym :S)]
  ;; use default constructor because we no longer have the merged accept-map
  (->NFA (into #{} )
         (merge-nfas-states
           abstract-nfa
           default-nfa
           if-nfa
           private-nfa
           this-nfa
           boolean-nfa
           do-nfa
           implements-nfa
           protected-nfa
           break-nfa
           double-nfa
           import-nfa
           public-nfa
           throws-nfa
           byte-nfa
           else-nfa
           instanceof-nfa
           return-nfa
           transient-nfa
           case-nfa
           extends-nfa
           int-nfa
           short-nfa
           try-nfa
           catch-nfa
           interface-nfa
           static-nfa
           void-nfa
           char-nfa
           finally-nfa
           long-nfa
           strictfp-nfa
           volatile-nfa
           class-nfa
           float-nfa
           native-nfa
           super-nfa
           while-nfa
           const-nfa
           for-nfa
           new-nfa
           switch-nfa
           continue-nfa
           goto-nfa
           package-nfa
           synchronized-nfa)
         stateS
         (merge-nfas
           (:accept-states abstract-nfa)
           (:accept-states default-nfa)
           (:accept-states if-nfa)
           (:accept-states private-nfa)
           (:accept-states this-nfa)
           (:accept-states boolean-nfa)
           (:accept-states do-nfa)
           (:accept-states implements-nfa)
           (:accept-states protected-nfa)
           (:accept-states break-nfa)
           (:accept-states double-nfa)
           (:accept-states import-nfa)
           (:accept-states public-nfa)
           (:accept-states throws-nfa)
           (:accept-states byte-nfa)
           (:accept-states else-nfa)
           (:accept-states instanceof-nfa)
           (:accept-states return-nfa)
           (:accept-states transient-nfa)
           (:accept-states case-nfa)
           (:accept-states extends-nfa)
           (:accept-states int-nfa)
           (:accept-states short-nfa)
           (:accept-states try-nfa)
           (:accept-states catch-nfa)
           (:accept-states interface-nfa)
           (:accept-states static-nfa)
           (:accept-states void-nfa)
           (:accept-states char-nfa)
           (:accept-states finally-nfa)
           (:accept-states long-nfa)
           (:accept-states strictfp-nfa)
           (:accept-states volatile-nfa)
           (:accept-states class-nfa)
           (:accept-states float-nfa)
           (:accept-states native-nfa)
           (:accept-states super-nfa)
           (:accept-states while-nfa)
           (:accept-states const-nfa)
           (:accept-states for-nfa)
           (:accept-states new-nfa)
           (:accept-states switch-nfa)
           (:accept-states continue-nfa)
           (:accept-states goto-nfa)
           (:accept-states package-nfa)
           (:accept-states synchronized-nfa))
         (merge-nfas
           (:transitions abstract-nfa)
           (:transitions default-nfa)
           (:transitions if-nfa)
           (:transitions private-nfa)
           (:transitions this-nfa)
           (:transitions boolean-nfa)
           (:transitions do-nfa)
           (:transitions implements-nfa)
           (:transitions protected-nfa)
           (:transitions break-nfa)
           (:transitions double-nfa)
           (:transitions import-nfa)
           (:transitions public-nfa)
           (:transitions throws-nfa)
           (:transitions byte-nfa)
           (:transitions else-nfa)
           (:transitions instanceof-nfa)
           (:transitions return-nfa)
           (:transitions transient-nfa)
           (:transitions case-nfa)
           (:transitions extends-nfa)
           (:transitions int-nfa)
           (:transitions short-nfa)
           (:transitions try-nfa)
           (:transitions catch-nfa)
           (:transitions interface-nfa)
           (:transitions static-nfa)
           (:transitions void-nfa)
           (:transitions char-nfa)
           (:transitions finally-nfa)
           (:transitions long-nfa)
           (:transitions strictfp-nfa)
           (:transitions volatile-nfa)
           (:transitions class-nfa)
           (:transitions float-nfa)
           (:transitions native-nfa)
           (:transitions super-nfa)
           (:transitions while-nfa)
           (:transitions const-nfa)
           (:transitions for-nfa)
           (:transitions new-nfa)
           (:transitions switch-nfa)
           (:transitions continue-nfa)
           (:transitions goto-nfa)
           (:transitions package-nfa)
           (:transitions synchronized-nfa)
           (make-transition-NFA [[stateS (:start abstract-nfa) e]
                                 [stateS (:start default-nfa) e]
                                 [stateS (:start if-nfa) e]
                                 [stateS (:start private-nfa) e]
                                 [stateS (:start this-nfa) e]
                                 [stateS (:start boolean-nfa) e]
                                 [stateS (:start do-nfa) e]
                                 [stateS (:start implements-nfa) e]
                                 [stateS (:start protected-nfa) e]
                                 [stateS (:start break-nfa) e]
                                 [stateS (:start double-nfa) e]
                                 [stateS (:start import-nfa) e]
                                 [stateS (:start public-nfa) e]
                                 [stateS (:start throws-nfa) e]
                                 [stateS (:start byte-nfa) e]
                                 [stateS (:start else-nfa) e]
                                 [stateS (:start instanceof-nfa) e]
                                 [stateS (:start return-nfa) e]
                                 [stateS (:start transient-nfa) e]
                                 [stateS (:start case-nfa) e]
                                 [stateS (:start extends-nfa) e]
                                 [stateS (:start int-nfa) e]
                                 [stateS (:start short-nfa) e]
                                 [stateS (:start try-nfa) e]
                                 [stateS (:start catch-nfa) e]
                                 [stateS (:start interface-nfa) e]
                                 [stateS (:start static-nfa) e]
                                 [stateS (:start void-nfa) e]
                                 [stateS (:start char-nfa) e]
                                 [stateS (:start finally-nfa) e]
                                 [stateS (:start long-nfa) e]
                                 [stateS (:start strictfp-nfa) e]
                                 [stateS (:start volatile-nfa) e]
                                 [stateS (:start class-nfa) e]
                                 [stateS (:start float-nfa) e]
                                 [stateS (:start native-nfa) e]
                                 [stateS (:start super-nfa) e]
                                 [stateS (:start while-nfa) e]
                                 [stateS (:start const-nfa) e]
                                 [stateS (:start for-nfa) e]
                                 [stateS (:start new-nfa) e]
                                 [stateS (:start switch-nfa) e]
                                 [stateS (:start continue-nfa) e]
                                 [stateS (:start goto-nfa) e]
                                 [stateS (:start package-nfa) e]
                                 [stateS (:start synchronized-nfa) e]]))
         (merge-nfas
           (:accept-priorities abstract-nfa)
           (:accept-priorities default-nfa)
           (:accept-priorities if-nfa)
           (:accept-priorities private-nfa)
           (:accept-priorities this-nfa)
           (:accept-priorities boolean-nfa)
           (:accept-priorities do-nfa)
           (:accept-priorities implements-nfa)
           (:accept-priorities protected-nfa)
           (:accept-priorities break-nfa)
           (:accept-priorities double-nfa)
           (:accept-priorities import-nfa)
           (:accept-priorities public-nfa)
           (:accept-priorities throws-nfa)
           (:accept-priorities byte-nfa)
           (:accept-priorities else-nfa)
           (:accept-priorities instanceof-nfa)
           (:accept-priorities return-nfa)
           (:accept-priorities transient-nfa)
           (:accept-priorities case-nfa)
           (:accept-priorities extends-nfa)
           (:accept-priorities int-nfa)
           (:accept-priorities short-nfa)
           (:accept-priorities try-nfa)
           (:accept-priorities catch-nfa)
           (:accept-priorities interface-nfa)
           (:accept-priorities static-nfa)
           (:accept-priorities void-nfa)
           (:accept-priorities char-nfa)
           (:accept-priorities finally-nfa)
           (:accept-priorities long-nfa)
           (:accept-priorities strictfp-nfa)
           (:accept-priorities volatile-nfa)
           (:accept-priorities class-nfa)
           (:accept-priorities float-nfa)
           (:accept-priorities native-nfa)
           (:accept-priorities super-nfa)
           (:accept-priorities while-nfa)
           (:accept-priorities const-nfa)
           (:accept-priorities for-nfa)
           (:accept-priorities new-nfa)
           (:accept-priorities switch-nfa)
           (:accept-priorities continue-nfa)
           (:accept-priorities goto-nfa)
           (:accept-priorities package-nfa)
           (:accept-priorities synchronized-nfa)))))

;; java keywords
;; https://www.student.cs.uwaterloo.ca/~cs444/joos.html

;; operators, can get the string later on
;; = assign, == EQ, <= LE, != NE

;; Booleans
(def boolean-nfa
  (let [stateS        (gensym :S)
        statet        (gensym :t)
        statetr       (gensym :tr)
        statetru      (gensym :tru)
        statetrue     (gensym :true)
        statef        (gensym :f)
        statefa       (gensym :fa)
        statefal      (gensym :fal)
        statefals     (gensym :fals)
        statefalse    (gensym :false)]
  (make-NFA (into #{} )
            #{stateS statet statetr statetru statetrue statef statefa statefal statefals statefalse}
            stateS
            {statetrue  (list :BOOLEAN 0)
             statefalse (list :BOOLEAN 0)}
            (make-transition-NFA [[stateS statet \t]
                                  [statet statetr \r]
                                  [statetr statetru \u]
                                  [statetru statetrue \e]
                                  [stateS statef \f]
                                  [statef statefa \a]
                                  [statefa statefal \l]
                                  [statefal statefals \s]
                                  [statefals statefalse \e]]))))



;; complete nfa from all of the individual RE nfas
;; boolean
;; int-literal
;; keywords
;; operators
(def complete-nfa
  (let [stateS (gensym :S)]
  ;; use default constructor because we no longer have the merged accept-map
  (->NFA (into #{} )
         (merge-nfas-states integer-literal-nfa operators-nfa)
         stateS
         (merge-nfas (:accept-states integer-literal-nfa) (:accept-states operators-nfa))
         (merge-nfas (:transitions integer-literal-nfa)
                     (:transitions operators-nfa)
                     (make-transition-NFA [[stateS (:start integer-literal-nfa) e]
                                           [stateS (:start operators-nfa) e]]))
         (merge-nfas (:accept-priorities integer-literal-nfa)
                     (:accept-priorities operators-nfa)))))
