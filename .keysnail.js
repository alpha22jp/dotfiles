// ========================== KeySnail Init File =========================== //

// この領域は, GUI により設定ファイルを生成した際にも引き継がれます
// 特殊キー, キーバインド定義, フック, ブラックリスト以外のコードは, この中に書くようにして下さい
// ========================================================================= //
//{{%PRESERVE%
// ここにコードを入力して下さい
//}}%PRESERVE%
// ========================================================================= //

// ========================= Special key settings ========================== //

key.quitKey              = "C-g";
key.helpKey              = "<f1>";
key.escapeKey            = "C-q";
key.macroStartKey        = "undefined";
key.macroEndKey          = "undefined";
key.universalArgumentKey = "Not defined";
key.negativeArgument1Key = "C--";
key.negativeArgument2Key = "C-M--";
key.negativeArgument3Key = "M--";
key.suspendKey           = "<f10>";

// ================================= Hooks ================================= //


hook.setHook('KeyBoardQuit', function (aEvent) {
    if (key.currentKeySequence.length) {
        return;
    }
    command.closeFindBar();
    var marked = command.marked(aEvent);
    if (util.isCaretEnabled()) {
        if (marked) {
            command.resetMark(aEvent);
        } else {
            if ("blur" in aEvent.target) {
                aEvent.target.blur();
            }
            gBrowser.focus();
            _content.focus();
        }
    } else {
        goDoCommand("cmd_selectNone");
    }
    if (KeySnail.windowType === "navigator:browser" && !marked) {
        key.generateKey(aEvent.originalTarget, KeyEvent.DOM_VK_ESCAPE, true);
    }
});


// ============================= Key bindings ============================== //

key.setGlobalKey('C-s', function (ev) {
    command.iSearchForwardKs(ev);
}, 'Emacs ライクなインクリメンタル検索', true);

key.setGlobalKey('C-r', function (ev) {
    command.iSearchBackwardKs(ev);
}, 'Emacs ライクな逆方向インクリメンタル検索', true);

key.setGlobalKey('<f3>', function () {
    getBrowser().mTabContainer.advanceSelectedTab(1, true);
}, 'ひとつ右のタブへ');

key.setGlobalKey('<f2>', function () {
    getBrowser().mTabContainer.advanceSelectedTab(-1, true);
}, 'ひとつ左のタブへ');

key.setGlobalKey('C-u', function () {
    command.focusToById("urlbar");
}, 'ロケーションバーへフォーカス', true);

key.setGlobalKey('C-e', function () {
    command.focusToById("searchbar");
}, '検索バーへフォーカス', true);

key.setViewKey('C-n', function (ev) {
    key.generateKey(ev.originalTarget, KeyEvent.DOM_VK_DOWN, true);
}, '一行スクロールダウン');

key.setViewKey('C-p', function (ev) {
    key.generateKey(ev.originalTarget, KeyEvent.DOM_VK_UP, true);
}, '一行スクロールアップ');

key.setViewKey('M-<', function () {
    goDoCommand("cmd_scrollTop");
}, 'ページ先頭へ移動', true);

key.setViewKey('M->', function () {
    goDoCommand("cmd_scrollBottom");
}, 'ページ末尾へ移動', true);

key.setEditKey('C-d', function () {
    goDoCommand("cmd_deleteCharForward");
}, '次の一文字削除');

key.setEditKey('C-h', function () {
    goDoCommand("cmd_deleteCharBackward");
}, '前の一文字を削除');

key.setCaretKey([['C-a'], ['^']], function (ev) {
    ev.target.ksMarked ? goDoCommand("cmd_selectBeginLine") : goDoCommand("cmd_beginLine");
}, 'キャレットを行頭へ移動');

key.setCaretKey([['C-e'], ['$'], ['M->'], ['G']], function (ev) {
    ev.target.ksMarked ? goDoCommand("cmd_selectEndLine") : goDoCommand("cmd_endLine");
}, 'キャレットを行末へ移動');

key.setCaretKey([['C-n'], ['j']], function (ev) {
    ev.target.ksMarked ? goDoCommand("cmd_selectLineNext") : goDoCommand("cmd_scrollLineDown");
}, 'キャレットを一行下へ');

key.setCaretKey([['C-p'], ['k']], function (ev) {
    ev.target.ksMarked ? goDoCommand("cmd_selectLinePrevious") : goDoCommand("cmd_scrollLineUp");
}, 'キャレットを一行上へ');

key.setCaretKey([['C-f'], ['l']], function (ev) {
    ev.target.ksMarked ? goDoCommand("cmd_selectCharNext") : goDoCommand("cmd_scrollRight");
}, 'キャレットを一文字右へ移動');

key.setCaretKey([['C-b'], ['h'], ['C-h']], function (ev) {
    ev.target.ksMarked ? goDoCommand("cmd_selectCharPrevious") : goDoCommand("cmd_scrollLeft");
}, 'キャレットを一文字左へ移動');

key.setCaretKey([['M-f'], ['w']], function (ev) {
    ev.target.ksMarked ? goDoCommand("cmd_selectWordNext") : goDoCommand("cmd_wordNext");
}, 'キャレットを一単語右へ移動');

key.setCaretKey([['M-b'], ['W']], function (ev) {
    ev.target.ksMarked ? goDoCommand("cmd_selectWordPrevious") : goDoCommand("cmd_wordPrevious");
}, 'キャレットを一単語左へ移動');

key.setCaretKey('J', function () {
    util.getSelectionController().scrollLine(true);
}, '画面を一行分下へスクロール');

key.setCaretKey('K', function () {
    util.getSelectionController().scrollLine(false);
}, '画面を一行分上へスクロール');

key.setCaretKey(',', function () {
    util.getSelectionController().scrollHorizontal(true);
    goDoCommand("cmd_scrollLeft");
}, '左へスクロール');

key.setCaretKey('.', function () {
    goDoCommand("cmd_scrollRight");
    util.getSelectionController().scrollHorizontal(false);
}, '右へスクロール');

key.setCaretKey('z', function (ev) {
    command.recenter(ev);
}, 'キャレットの位置までスクロール');

key.setCaretKey([['C-`'], ['C-@']], function (ev) {
    command.setMark(ev);
}, 'マークをセット', true);

key.setCaretKey(':', function (ev, arg) {
    shell.input(null, arg);
}, 'コマンドの実行', true);

key.setCaretKey('R', function () {
    BrowserReload();
}, '更新', true);

key.setCaretKey('B', function () {
    BrowserBack();
}, '戻る');

key.setCaretKey('F', function () {
    BrowserForward();
}, '進む');

key.setCaretKey(['C-x', 'h'], function () {
    goDoCommand("cmd_selectAll");
}, 'すべて選択', true);

key.setCaretKey('f', function () {
    command.focusElement(command.elementsRetrieverTextarea, 0);
}, '最初のインプットエリアへフォーカス', true);

key.setCaretKey('M-p', function () {
    command.walkInputElement(command.elementsRetrieverButton, true, true);
}, '次のボタンへフォーカスを当てる');

key.setCaretKey('M-n', function () {
    command.walkInputElement(command.elementsRetrieverButton, false, true);
}, '前のボタンへフォーカスを当てる');
