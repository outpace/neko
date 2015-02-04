package neko.ui.adapters;

import android.content.Context;
import android.database.Cursor;
import android.view.View;
import android.view.ViewGroup;
import android.widget.CursorAdapter;
import clojure.lang.IFn;
import neko.data.sqlite.TaggedCursor;

public class TaggedCursorAdapter extends CursorAdapter {

    private IFn createViewFn;
    private IFn updateViewFn;
    private IFn getDataFn;

    public TaggedCursorAdapter(Context context, TaggedCursor cursor,
                               IFn createViewFn, IFn updateViewFn, IFn getDataFn) {
        super(context, cursor);
        this.createViewFn = createViewFn;
        this.updateViewFn = updateViewFn;
        this.getDataFn = getDataFn;
    }

    @Override
    public View newView(Context context, Cursor cursor, ViewGroup parent) {
        return (View)createViewFn.invoke(context);
    }

    @Override
    public void bindView(View view, Context context, Cursor cursor) {
        updateViewFn.invoke(view, cursor, getDataFn.invoke(cursor));
    }

    public Object getItem(int position) {
        return getDataFn.invoke(super.getItem(position));
    }

}
