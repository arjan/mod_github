{% if m.acl.use.mod_import_github %}
<div class="control-group">
    <div class="controls">
        {% button class="btn" text=_"Run Github webhook" postback=`webhook` delegate=`mod_github` %}
        <span class="help-inline">{_ Run the webhook to do a git pull. _}</span>
    </div>
</div>
{% endif %}
