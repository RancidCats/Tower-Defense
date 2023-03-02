using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Bullet : MonoBehaviour , IProjectile
{
    public Rigidbody rb;
    public float speed;
    public float damage;
    private Transform _target;

    public void Update()
    {
        
    }
    public void MakeDamage()
    {
        Destroy(gameObject);
        Enemys e = _target.GetComponent<Enemys>();
        if (e != null)
        {
            e.TakeDamage(damage);
        }
    }
    public void Moving()
    {
        if (_target == null)
        {
            Destroy(gameObject);
            return;
        }
        Vector3 dir = _target.position - transform.position;
        float distanceFrame = speed * Time.deltaTime;
        if (dir.magnitude <= distanceFrame)
        {
            MakeDamage();
            return;
        }
        transform.Translate(dir.normalized * distanceFrame, Space.World);
    }
    public void Pursuit(Transform target)
    {
        _target = target;
    }
}
