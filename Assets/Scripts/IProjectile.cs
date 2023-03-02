using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public interface IProjectile
{
    void MakeDamage();
    void Moving();
    void Pursuit(Transform target);
}
