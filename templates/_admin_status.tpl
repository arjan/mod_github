{% if m.acl.use.mod_import_github %}
<div class="form-group">
    <div>
        {% button class="btn btn-default" text=_"Run Github webhook" postback=`webhook` delegate=`mod_github` %}
        <span class="help-block">{_ Run the webhook to do a git pull. _}</span>
    </div>
</div>
{% endif %}
