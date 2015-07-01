// requires underscore.js
var yesodDsl = function(defs, strings, config) {
    var formItemXtypes = {
        'integer': 'numberfield',
        'number': 'numberfield',
        'boolean': 'checkbox',
        'string': 'textfield',
        'day': 'datefield',
        'timeofday': 'timefield'
    };
    function defineModel(modelName, fields, proxy, cb) {
        Ext.define(modelName, {
            extend: 'Ext.data.Model',
            fields: _.map(fields, function (f) {
                var name = f.name,
                    mapping = undefined;

                if (name == 'length') {
                    mapping = name;
                    name += '_';
                }
                var r = {
                    name : name,
                    type : (f.optional || f.references) ? "auto" : f.type
                };
                if ('default' in f)
                    r.defaultValue = f['default'];
                if (mapping != undefined) {
                    r.mapping = mapping;
                }
                return r;
            }),
            proxy: proxy
        }, cb);

    }
    function createStore(storeCls, storeId) {
        var store = Ext.create(storeCls, { storeId:storeId });
        config.defaultStoreFilters.forEach(function(cf) {
            var f = {
                id: cf.field,
                property: cf.field,
                value: cf.value
            };
            if (cf.op)
                f.comparison = cf.op;
            store.addFilter(new Ext.util.Filter(f), false);
        });
        return store;
    }
    function initFormFilters(ffs, form) {
        return _.map(ffs, function (fInfo) {
            if (typeof fInfo == 'string') {
                fInfo = {
                    name: fInfo,
                    field: 'id'
                };
            }
            var record = form.getForm().getRecord();

            var value = ('value' in fInfo) ? fInfo.value : record.get(fInfo.field)  ;
            var f = {
                id: fInfo.name,
                property: fInfo.name,
                value: (value == null) ? null : (''+ ((value != "") ? value : 0))
            };
            if (fInfo.op)
                f.comparison = fInfo.op;
            return new Ext.util.Filter(f);
        });
    }
    function defaultRoute(entityName) {
        return _.find(defs.routes, function (r) {
            return r.path.length == 1 && r.path[0].type == "string"
                && r.handlers.some(function (h) {
                    return h.type == 'GET'
                        && h.outputs.some(function (o) {
                            return o.name == 'id' && o.references == entityName;
                        });
                })
        });
    }
    function entityByRoute(routeName) {
        var res = undefined;
        defs.routes.forEach(function (r) {
            if (r.path.length == 1 && r.path[0].type == "string" && r.path[0].value == routeName) {
                r.handlers.forEach(function (h) {
                    if (h.type == "GET") {
                        h.outputs.forEach(function (o) {
                            if (o.name == "id") {
                                res = o.references;
                            }
                        });
                    }
                });
            }
        });
        return res;
    }
    function createFromGrid(form, recordFields, formFields, srcGrid, gridFields, dstGrid) {
        var modelName = dstGrid.store.model.$className,
            entityName = entityByRoute(modelName.split(".").pop()),
            formRecord = form.getRecord(),
            formValues = form.getValues();

        var selected = srcGrid.getSelectionModel().getSelection();
        selected.forEach(function (s) { 
            var record = Ext.create(config.name + '.model.' + entityName, entityDefaults(entityName));
            recordFields.forEach(function (rf) {
                record.set(rf.dst, formRecord.get(rf.src));
            });
            formFields.forEach(function (ff) {
                record.set(ff.dst, formValues[ff.src])
            });

            gridFields.forEach(function (gf) {
                record.set(gf.dst, s.get(gf.src));
            });
            record.save({
               success: function(rec, op) {
                   var r = JSON.parse(op.response.responseText)
                   if (!record.getId()) {
                       record.setId(r.id);
                   }
                   dstGrid.store.add(record);
               }
            });
        });


    }

    function saveError(responseText) {
        Ext.Msg.alert(translate('saveError.title'), 
                     translate('saveError.message') + ": " + responseText);
    }

    function entityDefaults(entityName) {
        var e = _.find(defs.entities, function (e) { return e.name == entityName; }),
            r = {};
        e.fields.forEach(function (f) {
            if ('default' in f) {
                r[f.name] = f['default'];
            }
        });
        return r;
    }

    function gridWidgetName(routeName, gridCfg) {
        return gridCfg.widget || (routeName + 'list');
    }
    function refreshGrids(routeName) {
        var reloaded = [];
        var routeCfg = config.routes[routeName] || {};
        (routeCfg.grids || []) .forEach(function (gridCfg) {
            Ext.ComponentQuery.query(gridWidgetName(routeName, gridCfg))
                .forEach(function (g) { 
                    var s = g.store;
                    if (!reloaded.some(function (rs) { return rs === s; })) {
                        s.reload();
                        reloaded.push(s);
                    }
                });
        });
    }
    function findGrid(widgetName) {
        var res = undefined;
        _.each(config.routes, function (r) {
            r.grids.forEach(function (g) {
                if (g.widget == widgetName) 
                    res = g;
            });
        });
        return res;
    }
    function initFormItem(h, formCfg, widgetName) {
        return function (i) {             
            var itemCfg = {};
            if (typeof i == 'string') {
                itemCfg.name = i;
            } else {
                itemCfg = i;
            }

            var field = _.find(h.outputs, function (o) { return o.name == itemCfg.name; }) || {};
            if (!('xtype' in itemCfg)) {
                if (field.references) {
                    var entityRoute = defaultRoute(field.references);
                    if (entityRoute) {
                        itemCfg.xtype = routeInfo(entityRoute).name + 'combo';
                    } else {
                        itemCfg.xtype = field.references + 'combo';
                    }
                } else {
                    itemCfg.xtype = formItemXtypes[field.type];
                }
            }
            var res = {
                xtype:itemCfg.xtype,
                name: itemCfg.name,
                allowBlank: itemCfg.allowBlank || true,
                labelWidth: itemCfg.labelWidth || formCfg.labelWidth || config.formLabelWidth || 120,
                autoscroll: itemCfg.height ? true : false,
                flex: itemCfg.flex,
                param: itemCfg.param
            };
            if ('inputType' in itemCfg) {
                res.inputType = itemCfg.inputType;
            }
            if ('minLength' in itemCfg) {
                res.minLength = itemCfg.minLength;
                if (itemCfg.minLengthText)
                    res.minLengthText = translate(itemCfg.minLengthText);
            }

            if ('height' in itemCfg)
                res.height = itemCfg.height;

            if (field.type == "boolean")
                res.inputValue = true;

            var filters = (formCfg.filters || []).concat(itemCfg.filters || []);
            if (filters.length) {

                if (findGrid(itemCfg.xtype)) {
                    res.listeners = {
                        render: function(grid) {
                            initFormFilters(filters, grid.up('form')).forEach(function (f) {
                                grid.store.filters.removeAtKey(f.id);
                                grid.store.addFilter(f, false);
                            });
                            grid.store.reload();
                        }
                    };
                } 
            }
            if (itemCfg.bodyPadding) {
                res.bodyPadding = itemCfg.bodyPadding;
            }
            if (itemCfg.title) {
                res.title = translate(itemCfg.title);
            }
            if (itemCfg.layout) {
                res.layout = itemCfg.layout;
            }
            if (itemCfg.items) {
                res.items = _.map(itemCfg.items, function(i) { return initFormItem(h, formCfg, widgetName)(i); });
            }
            var text = itemCfg.text ? translate(itemCfg.text) : translate(widgetName + '.' + itemCfg.name);
            if (itemCfg.xtype == 'button') {
                res.text = text;
                res.listeners = {
                    click: function(button) {
                        if (itemCfg.action == "create") {
                            var form = button.up('form'),
                                srcGrid = form.down(itemCfg.srcGrid),
                                dstGrid = form.down(itemCfg.dstGrid);
                            createFromGrid(form.getForm(), itemCfg.recordFields || [], itemCfg.formFields || [], srcGrid, itemCfg.gridFields, dstGrid);

                        } 
                        if (itemCfg.form) {
                            var record = button.up('form').getForm().getRecord();
                            openFormWindow(itemCfg.form, 
                                itemCfg.formWidth || config.subFormWidth || 510,
                                itemCfg.formHeight || config.subFormHeight || 530,
                                record);
                        }
                    }
                };
            } else {
                res.fieldLabel = text;
            }
            return res;
        };
    }


    // customized ExtJS ComboBox to fetch a record by ID field (call configStore when value set)
    Ext.define(config.name + '.view.Combo',{
        extend: 'Ext.form.field.ComboBox',
        minChars : 1,
        typeAhead: true,
        store:'ext-empty-store',
        queryMode: 'remote',
        pageSize: config.comboPageSize || 10,
        valueField: 'id',
        displayField: 'name',
        plugins: config.defaultTextFieldPlugins || [],
        initComponent: function() {
            this.callParent(arguments);
            this.store = createStore(this.myStore);
        },
        configStore: function(extraFilters) {
            var v = this.getValue()
            this.store.addFilter(
                new Ext.util.Filter({
                    id: 'id',
                    property: 'id',
                    value: ''+v
                }), false
            );
            var c = this;
            c.store.load(function() {
                c.setValue(v);
                c.store.filters.removeAtKey('id');

                if (c.getFilters) {
                    var filters = c.getFilters();
                    for (var i = 0; i < filters.length; i++) {

                        c.store.addFilter(filters[i], false);
                    }
                }
                if (extraFilters) {
                    for (var i = 0; i < extraFilters.length; i++) {
                        c.store.addFilter(extraFilters[i], false);
                    }
                }
            });
        }
    });


    function endsWith(str, suffix) {
        return str.indexOf(suffix, str.length - suffix.length) !== -1;
    }
    function translate(k,d) {
        var parts = k.split(".");
        if (k in strings) {
            while (k in strings) {
                var pk = k;
                k = strings[k];
                if (pk == k)
                    break;
            }
            return k;
        } else if (parts[parts.length - 1] in strings) {
            k = parts[parts.length - 1];
            while (k in strings) {
                var pk = k;
                k = strings[k];
                if (pk == k)
                    break;
            }
            return k;
        } else if (d) {
            return d;
        } else {
            return k;
        }
    };

    function idTooltip(record) { 
        return "ID " + record.get('id');
    }

    function routeInfo(r) {
        var name = '', url = config.urlBase;
        r.path.forEach(function (pp) {
            url += "/";
            if (pp.type == "string") {
                name += pp.value;
                url += pp.value
            } else {
                name += pp.references;
                url += '(ID)';
            }
        }); 
        return {
            name:name,
            url:url
        };
    }
    function formFilters(formName) {
        var res = [];
        _.each(config.routes, function (r) {
            (r.forms || []).forEach(function (f) {
                if (f.widget == formName && 'filters' in f) {
                    res = f.filters;
                }
            });
        });
        return res;
    }
    function openFormWindow(formName, formWidth, formHeight, record) {
        var winId = formName + record.get('id');
        var win = Ext.getCmp(winId);
        if (win) {
            win.toFront();
        } else {
            win = new Ext.Window({
                id: winId,
                width: formWidth, 
                height: formHeight,
                resizable:false,
                autoScroll:true,
                title: translate(formName + ".title"),
                items: [ { xtype: formName } ]
            });
            var form = win.down(formName);
            form.loadRecord(record);
            win.query('combobox').forEach(function (cb) { 
                if ('configStore' in cb) {
                    cb.configStore(initFormFilters(formFilters(formName), form));
                }
            });

            win.show();

        }
    }
    defs.enums.forEach(function (e) {


        var storeName = config.name + '.store.' + e.name,
            comboName = config.name + '.view.' + e.name + '.Combo',
            widgetName = e.name + 'combo';

        Ext.define(storeName, {
                extend: 'Ext.data.Store',
                fields: [ 'text', 'value' ],
                data: {
                    values: _.map(e.values, function (v) {
                        return {
                            text: translate(e.name + "." + v),
                            value: v
                        };
                    })
                },
                proxy: {
                    type: 'memory',
                    reader: {
                        type: 'json',
                        root: 'values'
                    }
                }
            }, function (storeCls) {
            Ext.data.StoreManager.register(storeCls);
            Ext.define(comboName, {
                extend: 'Ext.form.ComboBox',
                store: Ext.create(storeCls, { storeId:e.name }),
                alias: 'widget.' + widgetName,
                queryMode: 'local',
                displayField: 'text',
                valueField: 'value',
                emptyText: translate(widgetName + '.emptyText')
            });
        });
    });
    defs.entities.forEach(function (e) {
        var modelName = config.name + '.model.' + e.name,
            route = defaultRoute(e.name);
        if (route) {
            var proxy = {
                type: 'rest',
                url: routeInfo(defaultRoute(e.name)).url,
                reader: {
                    type: 'json',
                    root: 'result',
                    totalProperty: 'totalCount'
                },
                listeners: {
                    exception: function (proxy, response, operation) {
                        if (response.request.options.method != 'GET')
                            saveError(response.responseText);
                    }
                }
            };
            defineModel(modelName, e.fields, proxy);

        }

    });
    defs.routes.forEach(function (r) {

        var info = routeInfo(r),
            name = info.name,
            url = info.url,
            proxyName = config.name + '.proxy.' + name,
            modelName = config.name + '.model.' + name,
            storeName = config.name + '.store.' + name,
            comboName = config.name + '.view.' + name + '.Combo';
        r.handlers.forEach(function (h) {
           
            var routeCfg = config.routes[name] || {};

            // create models and stores for GET handlers without parameters  
            if (h.type == "GET" && r.path.length == 1 && r.path[0].type == "string") {

                Ext.define(proxyName, {
                        extend: 'Ext.data.proxy.Rest',
                        type: 'rest',
                        url: url,
                        reader: {
                            type: 'json',
                            root: 'result',
                            totalProperty: 'totalCount'
                        },
                        listeners: {
                            exception: function (proxy, response, operation) {
                                if (response.request.options.method != 'GET')
                                    saveError(response.responseText);
                            }
                        },
                        encodeFilters: function(filters) {
                            var min = [],
                                length = filters.length,
                                i = 0;

                            for (; i < length; i++) {
                                min[i] = {
                                    property: filters[i].property,
                                    value   : filters[i].value
                                };
                                if ('comparison' in filters[i])
                                    min[i].comparison = filters[i].comparison;
                            }
                            return this.applyEncoding(min);
                        }
                }, function(proxyCls) {
                    var proxy = Ext.create(proxyCls);

                    defineModel(modelName, h.outputs, proxy, function(model) {
                        Ext.define(storeName, {
                            extend: 'Ext.data.Store',
                            model: modelName,
                            pageSize: config.defaultPageSize,
                            remoteFilter: true,
                            remoteSort: true,
                            proxy: proxy
                        }, function(storeClass) {
                            Ext.data.StoreManager.register(storeClass);


                            // ComboBox for entities with 'name' field
                            if (_.find(h.outputs, function (o) { return o.name == 'name'; }) || routeCfg.combo) {
                                var tpl = undefined, displayTpl = undefined, comboCfg = routeCfg.combo || {};

                                if (comboCfg.field) {
                                    tpl = Ext.create('Ext.XTemplate',
                                        '<tpl for=".">',
                                            '<div class="x-boundlist-item">{' + comboCfg.field + '}</div>',
                                        '</tpl>');
                                    displayTpl = Ext.create('Ext.XTemplate',
                                        '<tpl for=".">',
                                            '{' + comboCfg.field + '}',
                                        '</tpl>');
                                }

                                Ext.define(comboName, {
                                    extend: config.name + '.view.Combo',
                                    alias: 'widget.' + name + 'combo',
                                    tpl: tpl, 
                                    displayTpl: displayTpl,
                                    emptyText: translate(name + 'combo.emptyText'),
                                    myStore: storeName,
                                    getFilters: comboCfg.getFilters
                                });
                            } 

                            var globalStore = createStore(storeClass, name);
                            // grids on demand by config
                            var grids = routeCfg.grids || [],
                                forms = routeCfg.forms || [];


                            grids.forEach(function(gridCfg) {
                                var store = undefined;
                                if (gridCfg.globalStore == true) {
                                    store = globalStore;
                                } else {
                                    store = createStore(storeClass);
                                }


                                (gridCfg.filters || []).forEach(function (f) {
                                    store.addFilter(new Ext.util.Filter({
                                            id: f.field,
                                            property: f.field,
                                            value: f.value,
                                            comparison: f.op ? f.op : "eq" 
                                        }), false)
                                });
                                var widgetName = gridWidgetName(name, gridCfg);
                                var listName  = config.name + '.view.' + name + '.' + widgetName;
                                Ext.define(listName, {
                                    extend: 'Ext.grid.Panel',
                                    alias: 'widget.' + widgetName,
                                    multiSelect: true,
                                    store: store,
                                    allowDeselect : true,
                                    title: translate(widgetName + '.title'),
                                    requires: ['Ext.toolbar.Paging', 'Ext.ux.grid.FiltersFeature'],
                                    viewConfig: {
                                        listeners: {
                                            render: function(view) {
                                                var tooltip = gridCfg.tooltip || idTooltip;
                                                source.Utils.createToolTip(view, tooltip);

                                                if (gridCfg.preload != false) {
                                                    store.load();
                                                }
                                            },
                                            celldblclick: function(grid, td, cellIndex, record, tr, rowIndex, e, eOpts) {
                                                if (gridCfg.form) {
                                                    openFormWindow(gridCfg.form, 
                                                                   gridCfg.formWidth || config.formWidth || 610,
                                                                   gridCfg.formHeight || config.formHeight || 630,
                                                                   record);

                                                }
                                            }
                                        }
                                    },
                                    initComponent: function() {
                                        var grid = this;
                                        this.columns = _.map(gridCfg.columns, function(c) {
                                                            var field = undefined,
                                                                filterable = undefined,
                                                                renderer = undefined,
                                                                flex = undefined,
                                                                header = undefined;
                                                            if (typeof c == 'string') {
                                                                field = c;
                                                                header = c;
                                                            } else {
                                                                field = c.field;
                                                                filterable = c.filterable;
                                                                renderer = c.renderer;
                                                                flex = c.flex;
                                                                header = c.header || field;
                                                            }
                                                            var dataIndex = field;
                                                            if (dataIndex == "length")
                                                                dataIndex += "_";
        
                                                            return {
                                                                "header" : translate(widgetName + "." + header, header),
                                                                "dataIndex" : dataIndex,
                                                                "flex" : flex || 1,
                                                                "filterable" : filterable || true,
                                                                "renderer" : renderer
                                                            };
                                                        });
                                        var displayMsg = translate(widgetName + '.paging', true);
                                        if (displayMsg === true)
                                            displayMsg = translate(widgetName + '.title') + " {0} - {1} / {2}";
            
                                        this.bbar = Ext.create('Ext.PagingToolbar', {
                                            store: store,
                                            displayInfo: true,
                                            displayMsg: displayMsg,
                                            emptyMsg: translate(widgetName + '.emptyPaging'),
                                            items: ['-'].concat(_.map(gridCfg.bottomToolbar || [], function(tb) {
                                                return {
                                                    name:tb.name,
                                                    text:translate(widgetName +"." + tb.name),
                                                    listeners: {
                                                        click: function(button) {
                                                            if (tb.action == 'remove') {

                                                                var selected = button.up(widgetName).getSelectionModel().getSelection();

                                                                store.remove(selected);
                                                                store.sync();
                                                            } else if (tb.action == 'new') {
                                                                var record = Ext.create(modelName, entityDefaults(entityName));

                                                                if (tb.recordFields) {
                                                                    var form = button.up('form');
                                                                    if (form) {
                                                                        var formRecord = form.getForm().getRecord();
                                                                        tb.recordFields.forEach(function (ff) {
                                                                            if (typeof ff == "string") {
                                                                                ff = {
                                                                                    "src" : "id",
                                                                                    "dst" : ff
                                                                                };
                                                                            };
                                                                            record.set(ff.dst, formRecord.get(ff.src));
                                                                        });

                                                                    }
                                                                }
                                                                openFormWindow(gridCfg.form, 
                                                                               gridCfg.formWidth || config.formWidth || 610,
                                                                               gridCfg.formHeight || config.formHeight || 630,
                                                                               record);

                                                            }
                                                        }
                                                    }
                                                };
                                            }))
                                        });
                                        this.callParent(arguments);

                                    },
                                    features: [{ ftype : 'filters', encode:true}],
                                    dockedItems: (function () { 
                                        var toolbar = _.map(gridCfg.toolbar || [], function(tb) {
                                                if (endsWith(tb.xtype, 'combo')) {
                                                    return {
                                                        xtype: tb.xtype,
                                                        listeners: {
                                                            select: function(combo) {
                                                                store.filters.removeAtKey(tb.filterField);
                                                                store.addFilter(new Ext.util.Filter({
                                                                        id: tb.filterField,
                                                                        property: tb.filterField,
                                                                        value: ''+combo.getValue()
                                                                    }));
                                                            },
                                                            change: function(combo) {
                                                                if (combo.getValue() == '') {
                                                                    store.filters.removeAtKey(tb.filterField);
                                                                    store.reload();
                                                                }
                                                            }
                                                        }
                                                    };
                                                } else
                                                    console.log("unsupported toolbar item xtype: " + tb.xtype);
                                            });
                                        if (gridCfg.searchField != false) {
                                            toolbar.push({ 
                                                xtype: 'textfield',
                                                itemId: 'textSearch', 
                                                flex:1,
                                                emptyText: translate(widgetName + '.search'),
                                                listeners: {
                                                    change: {
                                                        buffer: 500,
                                                        fn: function(textField) {
                                                            store.filters.removeAtKey('query');
                                                            if (textField.getValue() != '') {
                                                                store.addFilter(new Ext.util.Filter({
                                                                    id: 'query',
                                                                    property: gridCfg.searchParam ? gridCfg.searchParam : 'query',
                                                                    value: '%' + textField.getValue() + '%',
                                                                    comparison: 'ilike'
                                                                }));
                                                            } else {
                                                                store.reload();
                                                            }
                                                        }
                                                    }
                                                },
                                                plugins: config.defaultTextFieldPlugins || []
                                            });
                                        }
                                        return toolbar;
                                    })()
                                });
                            });

                            var entityName = (_.find(h.outputs, function (o) { return o.name == "id"; }) || {}).references,
                                entityRoute = defaultRoute(entityName),
                                entityRouteInfo = entityName ? routeInfo(entityRoute) : undefined;
                            forms.forEach(function (formCfg) {

                                var widgetName = formCfg.widget || (name + 'form');
                                var formName  = config.name + '.view.' + name + '.' + widgetName;

                                var tabItems = _.map((formCfg.tabs || []), function (tabCfg) {
                                    return {
                                        xtype: 'panel',
                                        title: widgetName + "." + tabCfg.name,
                                        layout: {
                                            type: 'vbox',
                                            align: 'stretch'
                                        },
                                        bodyPadding: formCfg.bodyPadding || 5,
                                        items: tabCfg.items
                                    };
                                });
                                var tab = [];
                                if (tabItems.length) {
                                    tab = [{
                                        xtype: 'tabpanel',
                                        items: tabItems
                                    }];
                                }
                                var items = _.flatten(_.map(tab.concat(formCfg.items || []), initFormItem(h, formCfg, widgetName)));
                                
                                Ext.define(formName, {
                                    extend: 'Ext.form.Panel',
                                    alias: 'widget.' + widgetName,
                                    bodyPadding: formCfg.bodyPadding || (tabItems.length ? 0 : 5),
                                    layout: {
                                        type: 'vbox',
                                        align: 'stretch'
                                    },
                                    buttons: _.map(formCfg.buttons || [ 'save', 'saveandclose', 'closewithoutsaving' ], 
                                                   function (n) {
                                                       var btn;
                                                       if (typeof n == 'string') {
                                                           btn = {
                                                               name: n,
                                                               action: n
                                                           };
                                                       } else {
                                                           btn = n;
                                                       }
                                                       return { 
                                                           text: translate(widgetName + '.' + btn.name), 
                                                           name: btn.name,
                                                           listeners: {
                                                               click: function(button) {
                                                                   var win = button.up('window'),
                                                                       form = button.up('form').getForm();
                                                                       valid = form.isValid(),
                                                                       validationMessage = 'validationError.message';
                                                                   if (formCfg.validation) {
                                                                       var msg = formCfg.validation(form.getValues());
                                                                       if (msg) {
                                                                           validationMessage = msg;
                                                                           valid = false;
                                                                       }
                                                                   }

                                                                   var canClose = btn.action == 'closewithoutsaving' || valid;
                                                                    
                                                                   if (btn.action == 'save' || btn.action == 'saveandclose') {
                                                                       if (valid) {
                                                                           var store = Ext.getStore(entityRouteInfo.name),
                                                                               record = form.getRecord();
                                                                           if (btn.updateRecord != false) {
                                                                               form.updateRecord(record);
                                                                               h.outputs.forEach(function (o) {
                                                                                   if (o.references && record.get(o.name) == '') {
                                                                                       record.set(o.name, null);
                                                                                   }
                                                                               });
                                                                           }
                                                                           if (btn.url) {
                                                                               Ext.Ajax.request({
                                                                                   url: btn.url.replace('(ID)',''+record.getId()),
                                                                                   params: form.getValues(),
                                                                                   failure: function(request) {
                                                                                       saveError(request.responseText);
                                                                                   }
                                                                               });

                                                                           } else {
                                                                               record.save({
                                                                                   success: function(rec, op) {
                                                                                       var r = JSON.parse(op.response.responseText)
                                                                                       if (!record.getId()) {
                                                                                           record.setId(r.id);
                                                                                           refreshGrids(entityRouteInfo.name);
                                                                                       }
                                                                                   }
                                                                               });
                                                                           }
       
                                                                           


                                                                       } else {
                                                                           Ext.Msg.alert(translate('validationError.title'),
                                                                                         translate(validationMessage));
                                                                       }
                                                                   }
                                                                   if (canClose && (btn.action == 'saveandclose' || btn.action == 'closewithoutsaving' || btn.action == 'close')) {
                                                                       button.up('window').close();
                                                                   } 

                                                               }
                                                           }
                                                       };
                                                   }),
                                    items: items

                                });

                            });
                        });
                    });
                });
                    
                 
            } else if (h.type != "GET") {
               // TODO 

            }
            
        });
    });
    return {
        createFromGrid: createFromGrid
    };
};
