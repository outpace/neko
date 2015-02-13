package neko.ui.adapters;

import android.content.Context;
import android.database.Cursor;
import android.view.View;
import android.view.ViewGroup;
import android.widget.CursorAdapter;
import clojure.lang.IFn;
import clojure.lang.RT;
import neko.data.sqlite.TaggedCursor;

public class TaggedCursorAdapter extends CursorAdapter {

    private IFn createViewFn;
    private IFn updateViewFn;
    private IFn updateCursorFn;
    private IFn entityFromCursor;

    public TaggedCursorAdapter(Context context, IFn createViewFn,
                               IFn updateViewFn, Object cursorOrFn) {
        super(context, null);
        Cursor cursor;
        this.createViewFn = createViewFn;
        this.updateViewFn = updateViewFn;
        this.entityFromCursor = (IFn)RT.var("neko.data.sqlite", "entity-from-cursor").deref();
        if (cursorOrFn instanceof TaggedCursor)
            updateCursor((TaggedCursor)cursorOrFn);
        else {
            this.updateCursorFn = (IFn)cursorOrFn;
            updateCursor();
        }
    }

    @Override
    public View newView(Context context, Cursor cursor, ViewGroup parent) {
        return (View)createViewFn.invoke(context);
    }

    @Override
    public void bindView(View view, Context context, Cursor cursor) {
        updateViewFn.invoke(view, cursor, entityFromCursor.invoke(cursor));
    }

    public Object getItem(int position) {
        return entityFromCursor.invoke(super.getItem(position));
    }

    public void updateCursor() {
        if (updateCursorFn == null)
            throw new RuntimeException("Zero-argument updateCursor() needs adapter to be created with cursor-fn");
        updateCursor((TaggedCursor)updateCursorFn.invoke());
    }

    public void updateCursor(TaggedCursor cursor) {
        changeCursor(cursor);
        notifyDataSetChanged();
    }

}
